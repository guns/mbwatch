(ns mbwatch.connection-watcher
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
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        sig-notify-all sig-wait-alarm
                                        thread-loop update-period-and-alarm!]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable NOTICE
                                     WARNING defloggable log-with-timestamp!]]
            [mbwatch.network :refer [reachable?]]
            [mbwatch.types :as t :refer [StringList VOID Word]]
            [mbwatch.util :refer [human-duration join-sync-request]]
            [schema.core :as s :refer [Int defschema enum maybe pair]])
  (:import (clojure.lang Atom IFn)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)
           (mbwatch.command Command)
           (org.joda.time DateTime)))

(def ^:private ^:const RETRY-INTERVAL 15000)
(def ^:private ^:const TIME-JUMP-INTERVAL 60000)

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

(defloggable PendingSyncsEvent INFO
  [action         :- (enum :merge :release)
   mbchan->mboxes :- {String StringList}]
  (->> mbchan->mboxes
       join-sync-request
       (str (if (= action :merge)
              "Delaying syncs: "
              "Releasing pending syncs: "))))

(defloggable TimeJumpEvent WARNING
  [retry :- Int]
  (if (pos? retry)
    (format "Connection retry #%d in %s"
            retry
            (human-duration (quot (* retry RETRY-INTERVAL) 1000)))
    "Time jump! Retrying connections up to 3 times in the next 90 seconds."))

(defloggable ConnectionWatcherPreferenceEvent INFO
  [period :- Int]
  (str "Connection polling period set to " (human-duration (quot period 1000))))

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
  [log-chan     :- WritePort
   cmd-chan-out :- WritePort]
  (fn [_ _ old-conn-map new-conn-map]
    ;; Statuses are swapped in atomically, so don't mislead the user
    (-> (let [dt (DateTime.)]
          (reduce
            (fn [ps mbchan]
              (let [new-mbchan-map (new-conn-map mbchan)
                    old-mbchan-map (old-conn-map mbchan)
                    old-pending-syncs (:pending-syncs old-mbchan-map)
                    new-pending-syncs (:pending-syncs new-mbchan-map)]
                ;; Report altered pending syncs
                (when (and new-pending-syncs
                           (not= old-pending-syncs new-pending-syncs))
                  (put! log-chan (->PendingSyncsEvent :merge {mbchan new-pending-syncs})))
                (cond
                  ;; mbchan has been dissociated
                  (nil? new-mbchan-map)
                  (do (put! log-chan (ConnectionEvent. mbchan nil dt)) ps)
                  ;; status has not changed
                  (= (:status old-mbchan-map) (:status new-mbchan-map)) ps
                  :else
                  (do
                    ;; log status change
                    (put! log-chan (ConnectionEvent. mbchan (:status new-mbchan-map) dt))
                    ;; status changed from nil|false -> true
                    (if (and (true? (:status new-mbchan-map)) new-pending-syncs)
                      (assoc ps mbchan new-pending-syncs)
                      ps)))))
            {} (distinct (mapcat keys [old-conn-map new-conn-map]))))
        (as-> ps
          (when (seq ps)
            (put! log-chan (->PendingSyncsEvent :release ps))
            ;; Commands must be conveyed
            (>!! cmd-chan-out (->Command :sync ps)))))))

(declare process-command)
(declare watch-connections!)

(t/defrecord ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   cmd-chan-in            :- ReadPort
   cmd-chan-out           :- WritePort
   log-chan               :- WritePort
   connections            :- Atom ; ConnectionMap
   period                 :- AtomicLong
   alarm                  :- AtomicLong
   status                 :- AtomicBoolean
   exit-fn                :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    ;; Changes to this atom can happen from multiple threads
    (add-watch connections ::watch-conn-changes
               (watch-conn-changes-fn log-chan cmd-chan-out))
    (let [f (future-catch-print
              (watch-connections! this))
          c (thread-loop []
              (when (.get status)
                (when-some [cmd (<!! cmd-chan-in)]
                  (when-some [cmd' (process-command this cmd)]
                    ;; Commands must be conveyed
                    (>!! cmd-chan-out cmd'))
                  (recur))))]
      (assoc this :exit-fn
             #(do (.set status false)     ; Stop after current iteration
                  (sig-notify-all status) ; Trigger timer
                  (close! cmd-chan-in)    ; Unblock consumer
                  (remove-watch connections ::watch-conn-changes)
                  @f
                  (<!! c)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (dissoc this :exit-fn))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s ConnectionWatcher [period: %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (human-duration (quot (.get period) 1000))))))

(s/defn ^:private watch-connections! :- VOID
  "Poll and update connections in the connections atom. Notify `status` to
   trigger an early connection check, and set it to false to exit.

   If we wakeup a minute past our expected alarm, assume the machine has just
   woken from sleep. Retry the connection a few times in case the gateway
   interface comes back up in the next 90 seconds."
  [connection-watcher :- ConnectionWatcher]
  (let [{:keys [connections mbchan->IMAPCredential log-chan
                ^AtomicBoolean status
                ^AtomicLong period
                ^AtomicLong alarm]} connection-watcher]
    (loop [retry 0]
      (when (.get status)
        (sig-wait-alarm status alarm)
        (when (.get status)
          (let [time-jump? (when (> (- (System/currentTimeMillis) (.get alarm))
                                    TIME-JUMP-INTERVAL)
                             (put! log-chan (->TimeJumpEvent 0))
                             true)
                ;; Update connections, then check if they are all reachable
                up? (-> connections
                        (swap! #(update-connections % mbchan->IMAPCredential 2000)) ; FIXME
                        (as-> c (every? :status (vals c))))
                ;; Nested `if`s to ensure all leaves are primitive
                retry (if (and time-jump? (not up?))
                        1 ; Start retrying
                        (if (zero? retry)
                          0 ; Not retrying
                          (if (or up? (>= retry 3))
                            0 ; Stop retrying
                            (inc retry))))
                interval (if (pos? retry)
                           (do (put! log-chan (->TimeJumpEvent retry))
                               (* retry RETRY-INTERVAL))
                           (.get period))]
            (.set alarm (+ (System/currentTimeMillis) interval))
            (recur retry)))))))

(s/defn ^:private merge-pending-syncs :- (pair ConnectionMap "conn-map"
                                               {String StringList} "sync-req")
  "Merge sync-req into conn-map as :pending-syncs entries if the :status of
   the mbchan is false. Returns the new conn-map and the new sync-req with
   merged entries removed.

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
  [connection-watcher :- ConnectionWatcher
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
  [connection-watcher :- ConnectionWatcher
   command            :- Command]
  (case (:opcode command)
    :conn/trigger (do (sig-notify-all (:status connection-watcher))
                      command)
    :conn/set-period (let [{:keys [^AtomicLong period
                                   ^AtomicLong alarm
                                   status log-chan]} connection-watcher
                           new-period ^long (:payload command)]
                       (when (update-period-and-alarm! new-period period alarm)
                         (sig-notify-all status)
                         (put! log-chan (->ConnectionWatcherPreferenceEvent new-period)))
                       command)
    :conn/remove (let [{:keys [connections]} connection-watcher]
                   (apply swap! connections dissoc (:payload command))
                   command)
    :sync (partition-syncs connection-watcher command)
    command))

(s/defn ->ConnectionWatcher :- ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   period                 :- Int
   cmd-chan-in            :- ReadPort
   log-chan               :- WritePort]
  (strict-map->ConnectionWatcher
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan log-chan
     :connections (atom {})
     :period (AtomicLong. period)
     :alarm (AtomicLong. (System/currentTimeMillis))
     :status (AtomicBoolean. true)
     :exit-fn nil}))
