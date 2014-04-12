(ns mbwatch.mbsync.sync-timer
  "A SyncTimer is a simple Command middleware that periodically produces :sync
   Commands. The interval and command to be issued can be set by sending
   commands to the SyncTimer. The timer can also be triggered on command.

                      ┌───────────┐
      ─── Command ──▶ │ SyncTimer ├─── Command ──▶
                      └─────┬─────┘
                            │
                            │
                            └─── Loggable ──▶
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-loop sig-notify-all
                                        sig-wait-alarm thread-loop
                                        update-period-and-alarm!]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable
                                     defloggable log-with-timestamp!]]
            [mbwatch.types :as t :refer [StringList VOID]]
            [mbwatch.util :refer [human-duration join-sync-request]]
            [schema.core :as s :refer [Int either maybe]])
  (:import (clojure.lang Atom IFn)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)
           (mbwatch.command Command)))

(defloggable ^:private SyncTimerPreferenceEvent INFO
  [pref :- (either Int {String StringList})]
  (if (integer? pref)
    (if (zero? pref)
      "Sync timer disabled."
      (str "Sync timer period set to: " (human-duration pref)))
    (let [req (join-sync-request pref)]
      (if (seq req)
        (str "Sync timer request set to: " req)
        "Sync timer disabled."))))

(declare process-command)

(t/defrecord ^:private SyncTimer
  [cmd-chan-in       :- ReadPort
   cmd-chan-out      :- WritePort
   log-chan          :- WritePort
   sync-request-atom :- Atom ; {mbchan [mbox]}
   period            :- AtomicLong
   alarm             :- AtomicLong
   status            :- AtomicBoolean
   exit-fn           :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [f (future-loop []
              (when (.get status)
                (sig-wait-alarm alarm)
                (when (.get status)
                  (let [p (.get period)
                        sync-req @sync-request-atom]
                    (.set alarm (if (pos? p) (+ (System/currentTimeMillis) p) 0))
                    (when (seq sync-req)
                      (>!! cmd-chan-out (->Command :sync sync-req))))
                  (recur))))
          c (thread-loop []
              (when (.get status)
                (when-some [cmd (<!! cmd-chan-in)]
                  ;; Convey commands ASAP
                  (>!! cmd-chan-out cmd)
                  (process-command this cmd)
                  (recur))))]
      (assoc this :exit-fn
             #(do (.set status false)    ; Stop after current iteration
                  (sig-notify-all alarm) ; Trigger timer
                  @f
                  (<!! c)
                  (close! cmd-chan-out)  ; Close outgoing channels
                  (close! log-chan)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (dissoc this :exit-fn))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s SyncTimer [period: %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (human-duration (.get period))))))

(s/defn ->SyncTimer :- SyncTimer
  [sync-request :- {String StringList}
   cmd-chan-in  :- ReadPort
   period       :- Int]
  (strict-map->SyncTimer
    {:cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :sync-request-atom (atom sync-request)
     :period (AtomicLong. period)
     :alarm (AtomicLong. (System/currentTimeMillis))
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private process-command :- VOID
  [sync-timer :- SyncTimer
   command    :- Command]
  (case (:opcode command)
    :timer/trigger (sig-notify-all (:alarm sync-timer))
    :timer/set-period (let [{:keys [^AtomicLong period
                                    ^AtomicLong alarm
                                    log-chan]} sync-timer
                            new-period ^long (:payload command)]
                        (when (update-period-and-alarm! new-period period alarm)
                          (sig-notify-all alarm)
                          (put! log-chan (->SyncTimerPreferenceEvent (.get period)))))
    :timer/set-request (let [{:keys [sync-request-atom log-chan]} sync-timer
                             old-req @sync-request-atom
                             new-req (reset! sync-request-atom (:payload command))]
                         (when-not (= old-req new-req)
                           (put! log-chan (->SyncTimerPreferenceEvent new-req))))
    nil)
  nil)
