(ns mbwatch.sync-timer
  "A SyncTimer is a simple Command middleware that periodically produces :sync
   Commands. The interval and command to be issued can be set by sending
   commands to the SyncTimer. The timer can also be triggered on command.

                      ┌───────────┐
      ─── Command ──▶ │ SyncTimer ├─── Command ──▶
                      └─────┬─────┘
                            │
                            │
                            └──────── Loggable ──▶
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [->Timer CHAN-SIZE TimerAtom
                                        future-loop set-alarm!
                                        shutdown-future sig-notify-all
                                        sig-wait-timer thread-loop
                                        update-timer!]]
            [mbwatch.events :refer [->SyncTimerPreferenceEvent]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.time :refer [human-duration]]
            [mbwatch.types :as t :refer [MBMap MBMapAtom VOID]]
            [mbwatch.util :refer [when-seq]]
            [schema.core :as s :refer [Int maybe]])
  (:import (clojure.lang IFn)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.command Command)))

(def ^:const MIN-POS-PERIOD 5000)

(declare process-command)

(t/defrecord ^:private SyncTimer
  [cmd-chan-in       :- ReadPort
   cmd-chan-out      :- WritePort
   log-chan          :- WritePort
   sync-request-atom :- MBMapAtom
   timer-atom        :- TimerAtom
   status            :- AtomicBoolean
   exit-fn           :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [f (future-loop []
              (when (.get status)
                (sig-wait-timer timer-atom)
                (when (.get status)
                  (set-alarm! timer-atom)
                  (when-seq [sync-req @sync-request-atom]
                    (>!! cmd-chan-out (->Command :sync sync-req)))
                  (recur))))
          c (thread-loop []
              (when (.get status)
                (when-some [cmd (<!! cmd-chan-in)]
                  ;; Convey commands ASAP
                  (>!! cmd-chan-out cmd)
                  (process-command this cmd)
                  (recur))))]
      (assoc this :exit-fn
             #(do (.set status false)         ; Stop after current iteration
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
    (->LogItem this (format "%s SyncTimer [period: %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (human-duration (:period @timer-atom))))))

(s/defn ->SyncTimer :- SyncTimer
  [sync-req    :- MBMap
   cmd-chan-in :- ReadPort
   period      :- Int]
  (strict-map->SyncTimer
    {:cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :sync-request-atom (atom sync-req)
     :timer-atom (atom (->Timer period MIN-POS-PERIOD false))
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private process-command :- VOID
  [sync-timer :- SyncTimer
   command    :- Command]
  (case (:opcode command)
    :sync/trigger (sig-notify-all (:timer-atom sync-timer))
    :sync/period (let [{:keys [sync-request-atom timer-atom log-chan]} sync-timer
                       new-period ^long (:payload command)]
                   (when (update-timer! timer-atom new-period MIN-POS-PERIOD)
                     (sig-notify-all timer-atom)
                     (put! log-chan (->SyncTimerPreferenceEvent
                                      :period @timer-atom @sync-request-atom))))
    :sync/set (let [{:keys [sync-request-atom timer-atom log-chan]} sync-timer
                    old-req @sync-request-atom
                    new-req (reset! sync-request-atom (:payload command))]
                (when-not (= old-req new-req)
                  (put! log-chan (->SyncTimerPreferenceEvent
                                   :sync-request @timer-atom @sync-request-atom))))
    nil)
  nil)
