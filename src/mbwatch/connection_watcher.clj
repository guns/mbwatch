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

                        ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┐
                        ┊ ConnectionMapAtom ┊
                        └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘
                                  △
                                  │
                        ┌─────────┴─────────┐
        ─── Command ──▶ │ ConnectionWatcher ├─── Command ──▶
                        └─────────┬─────────┘
                                  │
                                  │
                                  └──────────── Loggable ──▶
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.set :refer [intersection]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command CommandSchema]]
            [mbwatch.concurrent :refer [->Timer CHAN-SIZE TimerAtom
                                        future-catch-print set-alarm!
                                        shutdown-future sig-notify-all
                                        sig-wait-timer thread-loop
                                        update-timer!]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.events :refer [->ConnectionWatcherPreferenceEvent
                                    ->PendingSyncsEvent ->TimeJumpEvent]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.network :refer [reachable?]]
            [mbwatch.time :refer [human-duration]]
            [mbwatch.types :as t :refer [ConnectionMap ConnectionMapAtom
                                         MBMap VOID Word tuple]]
            [mbwatch.util :refer [catch-print when-seq]]
            [schema.core :as s :refer [Int either maybe pair pred validate]])
  (:import (clojure.lang IFn)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.events ConnectionEvent)
           (org.joda.time DateTime)))

(def ^:const MIN-POS-PERIOD 5000)
(def ^:private ^:const RETRY-INTERVAL 15000)
(def ^:private ^:const TIME-JUMP-INTERVAL 60000)

(s/defn ^:private update-connections :- ConnectionMap
  "Update the :status entries conn-map by checking connections in parallel.
   When an mbchan goes from true -> false, its :pending-syncs value is reset
   to nil.

   If timeout-or-status is a boolean value, no network connections are
   attempted, and all relevant :status entries are set to the value.

   mbchans not present in mbchan->IMAPCredential are ignored."
  [conn-map               :- ConnectionMap
   mbchan->IMAPCredential :- {Word IMAPCredential}
   timeout-or-status      :- (either Int Boolean)]
  (->> conn-map
       (pmap (fn [[mbchan m]]
               (if-some [imap (mbchan->IMAPCredential mbchan)]
                 (let [status (if (integer? timeout-or-status)
                                (reachable? (:host imap) (:port imap) timeout-or-status)
                                timeout-or-status)]
                   ;; true -> false
                   (if (and (true? (:status m)) (false? status))
                     [mbchan (assoc m :status status :pending-syncs nil)]
                     [mbchan (assoc m :status status)]))
                 ;; Upstream is not an IMAP server
                 [mbchan m])))
       (into {})))

(s/defn ^:private pending-sync-changes :- (pair MBMap "pool"
                                                MBMap "release")
  [old-conn-map :- ConnectionMap
   new-conn-map :- ConnectionMap
   log-chan     :- WritePort
   cmd-chan-out :- WritePort]
  (let [dt (DateTime.)]
    (reduce
      (fn [[pool release] mbchan]
        (let [new-mbchan-map (new-conn-map mbchan)
              old-mbchan-map (old-conn-map mbchan)
              old-pending-syncs (:pending-syncs old-mbchan-map)
              new-pending-syncs (:pending-syncs new-mbchan-map)
              ;; Newly pooled pending syncs
              pool (if (and new-pending-syncs
                            (not= old-pending-syncs new-pending-syncs))
                     (assoc pool mbchan new-pending-syncs)
                     pool)
              release (cond
                        ;; mbchan has been dissociated
                        (nil? new-mbchan-map)
                        (do (put! log-chan (ConnectionEvent. mbchan nil dt))
                            release)
                        ;; status has not changed
                        (= (:status old-mbchan-map) (:status new-mbchan-map))
                        release
                        ;; status has changed
                        :else
                        (do
                          (put! log-chan (ConnectionEvent.
                                           mbchan (:status new-mbchan-map) dt))
                          ;; status changed from nil|false -> true
                          (if (and (true? (:status new-mbchan-map)) new-pending-syncs)
                            (assoc release mbchan new-pending-syncs)
                            release)))]
          [pool release]))
      [{} {}] (distinct (mapcat keys [old-conn-map new-conn-map])))))

(s/defn ^:private watch-conn-changes-fn :- IFn
  "Log changes in connection statuses and release pending syncs. Pending syncs
   are not flushed from the reference when released; they are expected to be
   reset to nil when an mbchan status lapses back to false."
  [log-chan     :- WritePort
   cmd-chan-out :- WritePort]
  (fn [_ _ old-conn-map new-conn-map]
    (catch-print
      (let [[pool release] (pending-sync-changes old-conn-map new-conn-map
                                                 log-chan cmd-chan-out)]
        (when (seq pool)
          (put! log-chan (->PendingSyncsEvent :pool pool)))
        (when (seq release)
          (put! log-chan (->PendingSyncsEvent :release release))
          ;; Commands must be conveyed
          (>!! cmd-chan-out (->Command :sync release)))))))

(declare filter-command)
(declare watch-connections!)

(t/defrecord ^:private ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   cmd-chan-in            :- ReadPort
   cmd-chan-out           :- WritePort
   log-chan               :- WritePort
   connections-atom       :- ConnectionMapAtom
   timer-atom             :- TimerAtom
   timeout                :- Int
   status                 :- AtomicBoolean
   exit-fn                :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    ;; Changes to this atom can happen from multiple threads
    (add-watch connections-atom ::watch-conn-changes
               (watch-conn-changes-fn log-chan cmd-chan-out))
    (let [f (future-catch-print
              (watch-connections! this))
          c (thread-loop []
              (when (.get status)
                (when-some [cmd (<!! cmd-chan-in)]
                  (when-some [cmd' (filter-command this cmd)]
                    ;; Commands must be conveyed
                    (>!! cmd-chan-out cmd'))
                  (recur))))]
      (assoc this :exit-fn
             #(do (.set status false)         ; Stop after current iteration
                  (remove-watch connections-atom ::watch-conn-changes)
                  (sig-notify-all timer-atom) ; Trigger timer
                  (shutdown-future f 100)
                  (<!! c)
                  (close! cmd-chan-out)       ; Close outgoing channels
                  (close! log-chan)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s ConnectionWatcher [period: %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (human-duration (:period @timer-atom))))))

(s/defn ->ConnectionWatcher :- ConnectionWatcher
  [connections-atom       :- ConnectionMapAtom
   mbchan->IMAPCredential :- {Word IMAPCredential}
   period                 :- Int
   timeout                :- Int
   cmd-chan-in            :- ReadPort]
  (strict-map->ConnectionWatcher
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :connections-atom connections-atom
     :timer-atom (atom (->Timer period MIN-POS-PERIOD false))
     :timeout timeout
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private handle-time-jump! :- Boolean
  "Sets :connections-atom to false, logs a TimeJumpEvent, and returns true if
   the interval between the current time and the :alarm time is greater than
   TIME-JUMP-INTERVAL."
  [connection-watcher :- ConnectionWatcher]
  (let [{:keys [connections-atom timer-atom log-chan mbchan->IMAPCredential]} connection-watcher]
    (if (and (seq @connections-atom)
             (> (- (System/currentTimeMillis) (:alarm @timer-atom)) TIME-JUMP-INTERVAL))
      ;; Set all connections to false; anything could have happened while the
      ;; machine was asleep
      (do (swap! connections-atom #(update-connections % mbchan->IMAPCredential false))
          (put! log-chan (->TimeJumpEvent 0 0))
          true)
      false)))

(s/defn ^:private watch-connections! :- VOID
  "Poll and update connections in the connections atom. Notify `status` to
   trigger an early connection check, and set it to false to exit.

   If we wakeup a minute past our expected alarm, assume the machine has just
   woken from sleep. Retry the connection a few times in case the gateway
   interface comes back up in the next 90 seconds."
  [connection-watcher :- ConnectionWatcher]
  (let [{:keys [connections-atom timer-atom mbchan->IMAPCredential log-chan
                timeout ^AtomicBoolean status]} connection-watcher]
    (loop [retry 0]
      (when (.get status)
        (sig-wait-timer timer-atom)
        (when (.get status)
          (let [time-jump? (handle-time-jump! connection-watcher)
                ;; Update connections, then check if they are all reachable
                up? (-> connections-atom
                        (swap! #(update-connections % mbchan->IMAPCredential timeout))
                        (as-> c (every? :status (vals c))))
                ;; Nested `if`s to ensure all leaves are primitive
                retry (if (and time-jump? (not up?))
                        1 ; Start retrying
                        (if (zero? retry)
                          0 ; Not retrying
                          (if (or up? (>= retry 3))
                            0 ; Stop retrying
                            (inc retry))))
                retry-ms (when (pos? retry)
                           (put! log-chan (->TimeJumpEvent retry RETRY-INTERVAL))
                           (* retry RETRY-INTERVAL))]
            (set-alarm! timer-atom retry-ms)
            (recur retry)))))))

(s/defn ^:private merge-pending-syncs :- (tuple ConnectionMap MBMap)
  "Merge sync-req into conn-map as :pending-syncs entries if the :status of
   the mbchan is false. Returns the new conn-map and the new sync-req with
   merged entries removed."
  [conn-map :- ConnectionMap
   sync-req :- MBMap]
  (reduce-kv
    (fn [[conn-map sync-req] mbchan mboxes]
      (if (false? (:status (conn-map mbchan)))
        [(update-in conn-map [mbchan :pending-syncs]
                    #(cond (= % #{}) %
                           (seq mboxes) (into (or % #{}) mboxes)
                           :else #{}))
         (dissoc sync-req mbchan)]
        [conn-map sync-req]))
    [conn-map sync-req] sync-req))

(s/defn ^:private update-connections-for-sync :- (pred #(and (validate ConnectionMap %)
                                                             (validate MBMap (::sync-req (meta %))))
                                                       "ConnectionMap with ::sync-req meta")
  "Update the connections in conn-map matching sync-req. New entries are
   created for unknown mbchans in sync-req.

   Returns a new conn-map with a sync-request map of released :pending-syncs
   attached to the ::sync-req key in its metadata. This is essentially the
   output of merge-pending-syncs, except that we are abusing the metadata
   system to avoid external synchronization from within a transaction."
  [conn-map               :- ConnectionMap
   sync-req               :- MBMap
   mbchan->IMAPCredential :- {Word IMAPCredential}
   timeout                :- Int]
  (let [mbchans-with-imap (intersection (set (keys sync-req))
                                        (set (keys mbchan->IMAPCredential)))
        ;; Merge existing conn-map over the sync conn-map to find new entries,
        ;; then merge pending syncs before checking connections to avoid
        ;; releasing a pending sync and then issuing the same sync.
        [sync-map sync-req] (-> mbchans-with-imap
                                (zipmap (repeat {:status true :pending-syncs nil}))
                                (merge (select-keys conn-map mbchans-with-imap))
                                (merge-pending-syncs sync-req))
        [sync-map sync-req] (-> sync-map
                                (update-connections mbchan->IMAPCredential timeout)
                                (merge-pending-syncs sync-req))]
    (-> conn-map
        (merge sync-map)
        (with-meta {::sync-req sync-req}))))

(s/defn ^:private partition-syncs :- (maybe CommandSchema)
  "Take a :sync Command and check its mbchan IMAP servers. If all servers are
   reachable, then return the Command as is.

   If all servers are _unreachable_, the sync requests are merged into the
   :pending-syncs entries of the connections map and nil is returned.

   If only some servers are unreachable, a new payload for the :sync with only
   available servers is returned, while the rest are merged as above.

   This function always updates the connection map with new connection
   statuses."
  [connection-watcher :- ConnectionWatcher
   sync-command       :- CommandSchema]
  (let [{:keys [mbchan->IMAPCredential timeout]} connection-watcher
        sync-req (:payload sync-command)
        ;; Most of the work needs to be done in the transaction; note that
        ;; while the TCP connects do IO, they do not change program state.
        sync-req' (-> (:connections-atom connection-watcher)
                      (swap! update-connections-for-sync
                             sync-req mbchan->IMAPCredential timeout)
                      meta
                      ::sync-req)]
    (cond
      (empty? sync-req') nil
      (= sync-req sync-req') sync-command
      :else (assoc sync-command :payload sync-req'))))

(s/defn ^:private filter-command :- (maybe CommandSchema)
  [connection-watcher :- ConnectionWatcher
   command            :- CommandSchema]
  (case (:opcode command)
    :conn/trigger (do (sig-notify-all (:timer-atom connection-watcher))
                      command)
    :conn/period (let [{:keys [timer-atom log-chan]} connection-watcher
                       new-period ^long (:payload command)]
                   (when (update-timer! timer-atom new-period MIN-POS-PERIOD)
                     (sig-notify-all timer-atom)
                     (put! log-chan (->ConnectionWatcherPreferenceEvent
                                      :period @(:timer-atom connection-watcher))))
                   command)
    :conn/remove (let [{:keys [connections-atom]} connection-watcher]
                   (when-seq [mbchans (:payload command)]
                     (apply swap! connections-atom dissoc mbchans))
                   command)
    :sync (partition-syncs connection-watcher command)
    command))
