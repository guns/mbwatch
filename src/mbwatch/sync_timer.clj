(ns mbwatch.sync-timer
  "A SyncTimer is a simple Command middleware that periodically produces :sync
   Commands. The SyncTimer can also be triggered on command.

                      ┌───────────┐
      ─── Command ──▶ │ SyncTimer ├─── Command ──▶
                      └─────┬─────┘
                            │
                            │
                            └──────── Loggable ──▶
  "
  (:require [clojure.core.async :refer [<!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command CommandSchema]]
            [mbwatch.concurrent :refer [->Timer CHAN-SIZE TimerAtom
                                        future-loop set-alarm!
                                        shutdown-future sig-notify-all
                                        sig-wait-timer thread-loop
                                        update-timer!]]
            [mbwatch.events :refer [->UserCommandFeedback]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.mbmap :refer [mbmap-disj mbmap-merge]]
            [mbwatch.time :refer [human-duration]]
            [mbwatch.types :as t :refer [MBMap MBMapAtom VOID]]
            [mbwatch.util :refer [when-seq]]
            [schema.core :as s :refer [Int maybe]])
  (:import (clojure.lang IFn)
           (java.util.concurrent.atomic AtomicBoolean)))

(def ^:const MIN-POS-PERIOD 5000)

(declare process-command)

(t/defrecord SyncTimer
  [cmd-chan-in   :- ReadPort
   cmd-chan-out  :- WritePort
   log-chan      :- WritePort
   sync-req-atom :- MBMapAtom
   timer-atom    :- TimerAtom
   status        :- AtomicBoolean
   exit-fn       :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [f (future-loop []
              (when (.get status)
                (sig-wait-timer timer-atom)
                (when (.get status)
                  (set-alarm! timer-atom)
                  (when-seq [sync-req @sync-req-atom]
                    (put! cmd-chan-out (->Command :sync sync-req)))
                  (recur))))
          c (thread-loop []
              (when (.get status)
                (when-some [cmd (<!! cmd-chan-in)]
                  (put! cmd-chan-out cmd)
                  (process-command this cmd)
                  (recur))))]
      (assoc this :exit-fn
             #(do (.set status false)         ; Stop after current iteration
                  (sig-notify-all timer-atom) ; Trigger timer
                  (shutdown-future f 100)
                  (<!! c)
                  (close! cmd-chan-out)       ; CLOSE cmd-chan-out
                  (close! log-chan)))))       ; CLOSE log-chan

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
     :cmd-chan-out (chan CHAN-SIZE) ; OPEN cmd-chan-out
     :log-chan (chan CHAN-SIZE)     ; OPEN log-chan
     :sync-req-atom (atom sync-req)
     :timer-atom (atom (->Timer period MIN-POS-PERIOD false))
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private alter-sync-req-atom! :- VOID
  [alter-fn   :- IFn
   sync-timer :- SyncTimer
   merge-fn   :- (maybe IFn)
   command    :- CommandSchema]
  (let [{:keys [sync-req-atom timer-atom log-chan]} sync-timer
        {:keys [payload]} command
        old-req @sync-req-atom
        new-req (if merge-fn
                  (alter-fn sync-req-atom merge-fn payload)
                  (alter-fn sync-req-atom payload))]
    (when-not (= old-req new-req)
      (put! log-chan (->UserCommandFeedback :sync/Δ @sync-req-atom)))
    nil))

(s/defn ^:private process-command :- VOID
  [sync-timer :- SyncTimer
   command    :- CommandSchema]
  (case (:opcode command)
    :sync/trigger (sig-notify-all (:timer-atom sync-timer))
    :sync/period (let [{:keys [sync-req-atom timer-atom log-chan]} sync-timer
                       new-period ^long (:payload command)]
                   (when (update-timer! timer-atom new-period MIN-POS-PERIOD)
                     (sig-notify-all timer-atom)
                     (put! log-chan (->UserCommandFeedback :sync/period @timer-atom))))
    :sync/add (alter-sync-req-atom!
                swap! sync-timer mbmap-merge command)
    :sync/remove (alter-sync-req-atom!
                   swap! sync-timer mbmap-disj command)
    :sync/set (alter-sync-req-atom!
                reset! sync-timer nil command)
    :sync/clear (alter-sync-req-atom!
                  swap! sync-timer (fn [m _] (empty m)) command)
    nil)
  nil)
