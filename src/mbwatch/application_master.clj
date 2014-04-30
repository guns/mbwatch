(ns mbwatch.application-master
  "
                  ┌─────────┐
                  │ Console │
                  └────┬────┘
                       │
                       │ Input
                       ▼
             ┌───────────────────┐
             │ ApplicationMaster │
             └───────┬───┬───────┘
                     │   │
             Command │   │ Loggable
                     ▼   ▼
                ┌─────────────┐
                │ Application │
                └─────────────┘
  "
  (:require [clojure.core.async :refer [chan close! put!]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.application :refer [->Application status-table]]
            [mbwatch.command :refer [CommandSchema OPCODE-HELP
                                     parse-command-input]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        sig-notify-all]]
            [mbwatch.config :refer [->Config DEFAULT-MBWATCHRC-PATH]]
            [mbwatch.config.mbsyncrc :refer [DEFAULT-MBSYNCRC-PATH]]
            [mbwatch.console :refer [tty? with-console-input]]
            [mbwatch.events :refer [->UserCommandFeedback]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [atom-of]]
            [schema.core :as s :refer [Any maybe]])
  (:import (clojure.lang IFn Keyword)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.application Application)))

(declare process-command)

(t/defrecord ApplicationMaster
  [application :- (atom-of Application "ApplicationAtom")
   status      :- AtomicBoolean
   exit-fn     :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! (:log-chan @application) this)
    (swap! application comp/start) ; START Application
    (let [f (future-catch-print
              (when (tty?)
                (try
                  (with-console-input line
                    (let [cmd (parse-command-input line)]
                      (if (string? cmd)
                        (put! (:log-chan @application)
                              (->UserCommandFeedback :parse-error cmd))
                        (process-command this cmd))))
                  (catch InterruptedException _)) ; We are expecting this
                (when (.get status)
                  ;; User closed input stream; let the main thread know that
                  ;; we are done
                  (.println System/err "Goodbye!")
                  (.set status false)
                  (sig-notify-all status))))]
      (assoc this :exit-fn
             #(do (.set status false)
                  (close! (:cmd-chan @application)) ; CLOSE cmd-chan
                  (close! (:log-chan @application)) ; CLOSE log-chan
                  (future-cancel f)                 ; Cancel the read on stdin
                  (comp/stop @application)))))      ; STOP Application

  (stop [this]
    (log-with-timestamp! (:log-chan @application) this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s ApplicationMaster"
                            (if exit-fn "↓ Stopping" "↑ Starting")))))

(s/defn ->ApplicationMaster :- ApplicationMaster
  "Construct an ApplicationMaster instance from an option-map as produced by
   mbwatch.cli/parse-argv!"
  [options :- {Keyword Any}]
  (let [mbs (:config options DEFAULT-MBSYNCRC-PATH)
        mbw (:mbwatch-config options DEFAULT-MBWATCHRC-PATH)
        config (->Config options mbs mbw)
        cmd-chan (chan CHAN-SIZE)  ; OPEN cmd-chan
        log-chan (chan CHAN-SIZE)] ; OPEN log-chan
    (strict-map->ApplicationMaster
      {:application (atom (->Application config cmd-chan log-chan))
       :status (AtomicBoolean. true)
       :exit-fn nil})))

(s/defn ^:private process-command :- Any
  [application-master :- ApplicationMaster
   command            :- CommandSchema]
  (case (:opcode command)
    ;; Handle top-level commands directly
    :app/help (do (.println System/err OPCODE-HELP)
                  true)
    :app/clear (do (let [{:keys [cache-atom log-chan]} @(:application application-master)]
                     (when cache-atom
                       (swap! cache-atom empty)
                       (put! log-chan (->UserCommandFeedback :app/clear))))
                   true)
    :app/status (do (.println System/err (status-table @(:application application-master)))
                    true)
    ; :app/reload
    ; :app/restart
    :app/quit nil
    ;; Convey everything else
    (do
      (put! (-> application-master :application deref :cmd-chan) command)
      true)))
