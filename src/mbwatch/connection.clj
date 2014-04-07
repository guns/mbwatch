(ns mbwatch.connection
  "A ConnectionWatcher is a Command middleware that partitions, pools, and
   releases :sync commands based on the network availability of IMAP servers.
   It also periodically polls known servers on a fixed schedule.

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
            [clojure.set :refer [intersection]]
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
            [mbwatch.types :as t :refer [StringList Word]]
            [schema.core :as s :refer [Int defschema enum maybe pair
                                       protocol]]
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

(t/defrecord PendingSyncsEvent
  [action         :- (enum :merge :release)
   mbchan->mboxes :- {String StringList}
   timestamp      :- DateTime]

  Loggable

  (log-level [_] INFO)

  (log-item [this]
    (let [msg (if (= action :merge)
                "Delaying syncs: "
                "Releasing pending syncs: ")
          mbargs (->> mbchan->mboxes
                      (mapv (partial apply join-mbargs))
                      (string/join \space))]
      (->LogItem this (str msg mbargs)))))

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
               (if-some [imap (mbchan->IMAPCredential mbchan)]
                 (let [status (reachable? (:host imap) (:port imap) timeout)]
                   ;; true -> false
                   (if (and (true? (:status m)) (false? status))
                     [mbchan (assoc m :status status :pending-syncs nil)]
                     [mbchan (assoc m :status status)]))
                 ;; Upstream is not an IMAP server
                 [mbchan m])))
       (into {})))

(s/defn ^:private watch-conn-changes-fn :- IFn
  "Log changes in connection statuses and release pending syncs. Pending syncs
   are not flushed from the reference when released; they are expected to be
   reset to nil when an mbchan status lapses back to false."
  [log-chan    :- WritePort
   output-chan :- WritePort]
  (fn [_ _ old-conn-map new-conn-map]
    ;; Statuses are swapped in atomically, so don't mislead the user
    (-> (reduce
          (fn [ps mbchan]
            (let [new-mbchan-map (new-conn-map mbchan)
                  old-mbchan-map (old-conn-map mbchan)
                  old-pending-syncs (:pending-syncs old-mbchan-map)
                  new-pending-syncs (:pending-syncs new-mbchan-map)]
              ;; Report altered pending syncs
              (when (and new-pending-syncs
                         (not= old-pending-syncs new-pending-syncs))
                (log! log-chan (PendingSyncsEvent. :merge {mbchan new-pending-syncs} nil)))
              (cond
                ;; mbchan has been dissociated
                (nil? new-mbchan-map)
                (do (log! log-chan (ConnectionEvent. mbchan nil nil)) ps)
                ;; status has not changed
                (= (:status old-mbchan-map) (:status new-mbchan-map)) ps
                :else
                (do
                  ;; log status change
                  (log! log-chan (ConnectionEvent. mbchan (:status new-mbchan-map) nil))
                  ;; status changed from nil|false -> true
                  (if (and (true? (:status new-mbchan-map)) new-pending-syncs)
                    (assoc ps mbchan new-pending-syncs)
                    ps)))))
          {} (distinct (mapcat keys [old-conn-map new-conn-map])))
        (as-> ps
          (when (seq ps)
            (log! log-chan (PendingSyncsEvent. :release ps nil))
            ;; Commands must pass through
            (>!! output-chan (->Command :sync ps)))))))

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

(s/defn ^:private merge-pending-syncs :- (pair ConnectionMap "conn-map"
                                               {String StringList} "sync-req")
  "Merge sync-req into conn-map as :pending-syncs entries if the :status of
   the mbchan is false. Returns the new conn-map and the sync-req with merged
   entries removed.

   An mbox argument of [] resets the :pending-syncs value to #{} with
   the :all-mboxes? metadata flag set. Correspondingly, additions to a
   :pending-syncs value that has :all-mboxes? is ignored since a full mbchan
   sync will be issued once the server is reachable."
  [conn-map :- ConnectionMap
   sync-req :- {String StringList}]
  (reduce-kv
    (fn [[conn-map sync-req] mbchan mboxes]
      (if (false? (:status (conn-map mbchan)))
        [(update-in conn-map [mbchan :pending-syncs]
                    #(cond
                       (:all-mboxes? (meta %)) %
                       (seq mboxes) (into (or % #{}) mboxes)
                       :else (with-meta #{} {:all-mboxes? true})))
         (dissoc sync-req mbchan)]
        [conn-map sync-req]))
    [conn-map sync-req] sync-req))

(s/defn ^:private partition-syncs :- (maybe Command)
  "Take a :sync Command and check its mbchan IMAP servers. If all servers are
   reachable, then return the Command as is.

   If all servers are _unreachable_, the sync requests are merged into the
   :pending-syncs entries of the connections map and nil is returned.

   If only some servers are unreachable, a new payload for the :sync with only
   available servers is returned, while the rest are merged as above.

   This function always updates the connection map with new connection
   statuses."
  [connection-watcher :- (class-schema ConnectionWatcher)
   sync-command       :- Command]
  (let [{:keys [mbchan->IMAPCredential]} connection-watcher
        sync-req (:payload sync-command)
        mbchans-with-imap (intersection (set (keys sync-req))
                                        (set (keys mbchan->IMAPCredential)))
        ;; This is merged under the existing conn-map to introduce new mbchans
        new-map (zipmap mbchans-with-imap
                        (repeat {:status true :pending-syncs nil}))
        ;; Most of the work needs to be done in the transaction; note that
        ;; while the TCP scans do IO, they do not change any program state.
        conn (swap! (:connections connection-watcher)
                    ;; Merge pending syncs before checking connections to
                    ;; avoid releasing a sync and then doing the same sync
                    #(let [[conn sync] (-> (merge new-map %)
                                           (merge-pending-syncs sync-req))
                           [conn sync] (-> conn
                                           (update-connections mbchan->IMAPCredential 2000) ; FIXME: config
                                           (merge-pending-syncs sync))]
                       ;; This is an abuse of metadata, but it's better than
                       ;; dealing with synchronization issues…
                       (with-meta conn {::sync-req sync})))
        sync-req' (-> conn meta ::sync-req)]
    (cond
      (empty? sync-req') nil
      (= sync-req sync-req') sync-command
      :else (assoc sync-command :payload sync-req'))))

(s/defn ^:private process-command :- (maybe Command)
  [connection-watcher :- (class-schema ConnectionWatcher)
   command            :- Command]
  (case (:opcode command)
    :check-conn (do (sig-notify-all (:status connection-watcher))
                    (put! (:log-chan connection-watcher) command)
                    nil)
    ;; This is not the final consumer of :sync, so don't log it
    :sync (partition-syncs connection-watcher command)
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
