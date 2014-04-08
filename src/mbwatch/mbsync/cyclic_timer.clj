(ns mbwatch.mbsync.cyclic-timer
  "A CyclicTimer is a simple Command middleware that periodically produces
   :sync Commands. The interval and command to be issued can be set by sending
   commands to the CyclicTimer. The timer can also be triggered on command.

                      ┌─────────────┐
      ─── Command ──▶ │ CyclicTimer ├─── Command ──▶
                      └─────────────┘
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-loop sig-notify-all
                                        sig-wait-and-set-forward thread-loop
                                        update-period-and-alarm!]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable log!]]
            [mbwatch.types :as t :refer [StringList VOID]]
            [mbwatch.util :refer [human-duration]]
            [schema.core :as s :refer [Int maybe]])
  (:import (clojure.lang Atom IFn)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)
           (mbwatch.command Command)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(declare process-command)

(t/defrecord CyclicTimer
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
    (log! log-chan this)
    (let [f (future-loop []
              (when (.get status)
                (sig-wait-and-set-forward status period alarm)
                (when (.get status)
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
             #(do (.set status false)     ; Stop after current iteration
                  (sig-notify-all status) ; Trigger timer
                  (close! cmd-chan-in)    ; Unblock consumer
                  @f
                  (<!! c)))))

  (stop [this]
    (log! log-chan this)
    (exit-fn)
    (dissoc this :exit-fn))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s CyclicTimer [period: %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (human-duration (quot (.get period) 1000))))))

(s/defn ->CyclicTimer :- CyclicTimer
  [sync-request :- {String StringList}
   cmd-chan-in  :- ReadPort
   log-chan     :- WritePort
   period       :- Int]
  (strict-map->CyclicTimer
    {:cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan log-chan
     :sync-request-atom (atom sync-request)
     :period (AtomicLong. period)
     :alarm (AtomicLong. 0)
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private process-command :- VOID
  [cyclic-timer :- CyclicTimer
   command      :- Command]
  (case (:opcode command)
    :timer/trigger (sig-notify-all (:status cyclic-timer))
    :timer/set-period (let [{:keys [^AtomicLong period
                                    ^AtomicLong alarm
                                    status log-chan]} cyclic-timer
                            new-period ^long (:payload command)]
                        (when (update-period-and-alarm! new-period period alarm)
                          (sig-notify-all status)
                          (let [msg (str "Next timer period set to "
                                         (human-duration (quot new-period 1000)))]
                            (put! log-chan (LogItem. INFO (DateTime.) msg)))))
    :timer/set-request (let [{:keys [sync-request-atom log-chan]} cyclic-timer
                             old-req @sync-request-atom
                             new-req (reset! sync-request-atom (:payload command))]
                         (when-not (= old-req new-req)
                           (let [msg (str "Timer request set to: " new-req)]
                             (put! log-chan (LogItem. INFO (DateTime.) msg)))))
    nil)
  nil)
