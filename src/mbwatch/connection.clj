(ns mbwatch.connection
  "A ConnectionWatcher is a Command middleware that partitions, pools, and
   releases :sync commands based on the network availability of IMAP servers.

   Each time a :sync command is received, a TCP connection is attempted to
   each mbchan's IMAP server. If all mbchans are available, the command is
   conveyed unaltered.

   If some of the mbchans are unavailable, the payload is partitioned by
   server availability, and a new :sync command with only the reachable
   servers is created and conveyed (if any).

   The unreachable :sync entries are merged into a pool of pending syncs which
   are released as each respective server becomes available.

                        ┌───────────────────┐
        ─── Command ──▶ │ ConnectionWatcher ├─── Command ──▶
                        └─────────┬─────────┘
                                  │
                                  │
                                  └─── Loggable ──▶
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        sig-notify-all
                                        sig-wait-and-set-forward thread-loop]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable NOTICE
                                     WARNING log!]]
            [mbwatch.mbsync.events :refer [join-mbargs]]
            [mbwatch.network :refer [reachable?]]
            [mbwatch.types :as t :refer [Word]]
            [schema.core :as s :refer [Int defschema maybe protocol]]
            [schema.utils :refer [class-schema]])
  (:import (clojure.lang Atom IFn)
           (java.util.concurrent Future)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)
           (mbwatch.command Command)
           (org.joda.time DateTime)))

(t/defrecord ConnectionEvent
  [mbchan    :- String
   status    :- (maybe Boolean)
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (case status
      true  NOTICE
      false WARNING
      nil   INFO))

  (log-item [this]
    (let [msg (str "Channel " mbchan (case status
                                       true  " → reachable"
                                       false " ✖ unreachable"
                                       nil   " ∅ unregistered"))]
      (->LogItem this msg))))

(t/defrecord ReleasePendingSyncsEvent
  [mbchan->mboxes :- {String [String]}
   timestamp      :- DateTime]

  Loggable

  (log-level [_] INFO)

  (log-item [this]
    (let [msg (->> mbchan->mboxes
                   (mapv (partial apply join-mbargs))
                   (string/join \space)
                   (str "Releasing pending syncs: "))]
      (->LogItem this msg))))

(defschema ^:private ConnectionMap
  {String {:status Boolean
           :pending-syncs (maybe #{String})}})

(s/defn ^:private update-connections :- ConnectionMap
  "Update the :status entries connection-map by checking connections in
   parallel. When an mbchan goes from true -> false, its :pending-syncs value
   is reset to nil.

   mbchans not present in mbchan->IMAPCredential are ignored."
  [connection-map         :- ConnectionMap
   mbchan->IMAPCredential :- {Word IMAPCredential}
   timeout                :- Int]
  (->> connection-map
       (pmap (fn [[mbchan m]]
               (if-let [imap (mbchan->IMAPCredential mbchan)]
                 [mbchan (assoc m :status (reachable? (:host imap) (:port imap) timeout))]
                 [mbchan m])))
       (into {})))

(s/defn ^:private watch-conn-changes-fn :- IFn
  "Log changes in connection statuses and release pending syncs. Pending syncs
   are not flushed from the reference when released; they are expected to be
   reset to nil when an mbchan status lapses back to false."
  [log-chan    :- WritePort
   output-chan :- WritePort]
  (fn [_ _ old-map new-map]
    ;; Statuses are swapped in atomically, so don't mislead the user
    (let [dt (DateTime.)]
      (-> (reduce
            (fn [m mbchan]
              (let [nconn (new-map mbchan)]
                (cond
                  ;; mbchan has been dissociated
                  (nil? nconn) (do (put! log-chan (ConnectionEvent. mbchan nil dt)) m)
                  ;; status has not changed
                  (= (:status (old-map mbchan)) (:status nconn)) m
                  :else
                  (do
                    ;; log status change
                    (put! log-chan (ConnectionEvent. mbchan (:status nconn) dt))
                    ;; status changed from nil|false -> true; assoc pending syncs
                    (if (and (:status nconn) (:pending-syncs nconn))
                      (assoc m mbchan (vec (:pending-syncs nconn)))
                      m)))))
            {} (distinct (mapcat keys [old-map new-map])))
          (as-> ps
            (when (seq ps)
              (let [ev (ReleasePendingSyncsEvent. ps (DateTime.))
                    cmd (->Command :sync ps)]
                (put! log-chan ev)
                (put! log-chan cmd)
                ;; Commands must pass through
                (>!! output-chan cmd))))))))

(declare process-command)

(t/defrecord ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   connections            :- Atom ; ConnectionMap
   poll-ms                :- AtomicLong
   input-chan             :- ReadPort
   output-chan            :- WritePort
   log-chan               :- WritePort
   next-check             :- AtomicLong
   status                 :- AtomicBoolean
   exit-future            :- (maybe Future)
   exit-chan              :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (add-watch connections ::watch-conn-changes
               (watch-conn-changes-fn log-chan output-chan))
    (assoc this
           :exit-future
           (future-catch-print
             (loop []
               (when (.get status)
                 (sig-wait-and-set-forward status next-check poll-ms)
                 (swap! connections #(update-connections % mbchan->IMAPCredential 2000)) ; FIXME: Move to config
                 (recur))))
           :exit-chan
           (thread-loop []
             (when (.get status)
               (when-some [cmd (<!! input-chan)]
                 (when-some [cmd' (process-command this cmd)]
                   ;; Commands must pass through
                   (>!! output-chan cmd'))
                 (recur))))))

  (stop [this]
    ;; Exit ASAP
    (.set status false)
    (log! log-chan this)
    (sig-notify-all status)
    (close! input-chan)
    (remove-watch connections ::watch-conn-changes)
    @exit-future
    (<!! exit-chan)
    (dissoc this :exit-chan :exit-future))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-chan
                      "↓ Stopping ConnectionWatcher"
                      "↑ Starting ConnectionWatcher"))))

(s/defn ^:private partition-syncs :- (maybe Command)
  [connection-watcher :- (class-schema ConnectionWatcher)
   mbchan->mboxes :- {String [String]}]
  ;; XXX
  )

(s/defn ^:private process-command :- (maybe Command)
  [connection-watcher :- (class-schema ConnectionWatcher)
   command            :- Command]
  (case (:opcode command)
    :check-conn (do (sig-notify-all (:status connection-watcher))
                    (put! (:log-chan connection-watcher) command)
                    nil)
    ;; This is not the final consumer of :sync, so don't log it
    :sync (partition-syncs connection-watcher (:payload command))
    command))

(s/defn ->ConnectionWatcher :- ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   poll-ms                :- Int
   input-chan             :- ReadPort
   log-chan               :- WritePort]
  (strict-map->ConnectionWatcher
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :connections (atom {})
     :poll-ms (AtomicLong. poll-ms)
     :input-chan input-chan
     :output-chan (chan CHAN-SIZE)
     :log-chan log-chan
     :next-check (AtomicLong. 0)
     :status (AtomicBoolean. true)
     :exit-future nil
     :exit-chan nil}))
