(ns mbwatch.sync-timer
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
            [mbwatch.concurrent :refer [->Timer CHAN-SIZE TimerAtom
                                        future-loop set-alarm!
                                        shutdown-future sig-notify-all
                                        sig-wait-timer thread-loop
                                        update-timer!]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable
                                     defloggable log-with-timestamp!]]
            [mbwatch.types :as t :refer [SyncRequest VOID atom-of]]
            [mbwatch.util :refer [human-duration join-sync-request]]
            [schema.core :as s :refer [Int defschema enum maybe]])
  (:import (clojure.lang IFn)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.command Command)))

(def ^:private ^:const MIN-POS-PERIOD 5000)

(defschema ^:private SyncRequestAtom
  (atom-of SyncRequest "SyncRequestAtom"))

(declare process-command)

(t/defrecord ^:private SyncTimer
  [cmd-chan-in       :- ReadPort
   cmd-chan-out      :- WritePort
   log-chan          :- WritePort
   sync-request-atom :- SyncRequestAtom
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
                  (let [sync-req @sync-request-atom]
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
             #(do (.set status false)         ; Stop after current iteration
                  (sig-notify-all timer-atom) ; Trigger timer
                  (shutdown-future f 100)
                  (<!! c)
                  (close! cmd-chan-out)       ; Close outgoing channels
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
                            (human-duration (:period @timer-atom))))))

(s/defn ->SyncTimer :- SyncTimer
  [sync-req    :- SyncRequest
   cmd-chan-in :- ReadPort
   period      :- Int]
  (strict-map->SyncTimer
    {:cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :sync-request-atom (atom sync-req)
     :timer-atom (atom (->Timer period MIN-POS-PERIOD true))
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(defloggable ^:private SyncTimerPreferenceEvent INFO
  [sync-timer :- SyncTimer
   type       :- (enum :period :sync-request)]
  (let [{:keys [timer-atom sync-request-atom]} sync-timer
        period (:period @timer-atom)
        sync-req @sync-request-atom]
    (case type
      :period (if (zero? period)
                "Sync timer disabled."
                (str "Sync timer period set to: " (human-duration period)))
      :sync-request (if (seq sync-req)
                      (str "Sync timer request set to: " (join-sync-request sync-req))
                      "Sync timer disabled."))))

(s/defn ^:private process-command :- VOID
  [sync-timer :- SyncTimer
   command    :- Command]
  (case (:opcode command)
    :timer/trigger (sig-notify-all (:timer-atom sync-timer))
    :timer/set-period (let [{:keys [timer-atom log-chan]} sync-timer
                            new-period ^long (:payload command)]
                        (when (update-timer! timer-atom new-period MIN-POS-PERIOD)
                          (sig-notify-all timer-atom)
                          (put! log-chan (->SyncTimerPreferenceEvent sync-timer :period))))
    :timer/set-request (let [{:keys [sync-request-atom log-chan]} sync-timer
                             old-req @sync-request-atom
                             new-req (reset! sync-request-atom (:payload command))]
                         (when-not (= old-req new-req)
                           (put! log-chan (->SyncTimerPreferenceEvent sync-timer :sync-request))))
    nil)
  nil)
