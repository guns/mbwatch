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
  (:require [clojure.core.async :refer [>!! chan close! put!]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.application :refer [->Application]]
            [mbwatch.command :refer [OPCODE-HELP parse-command-input]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        shutdown-future]]
            [mbwatch.config :refer [->Config DEFAULT-CONFIG-PATH]]
            [mbwatch.config.mbsyncrc :refer [DEFAULT-MBSYNCRC-PATH]]
            [mbwatch.console :refer [tty? with-console-input]]
            [mbwatch.events :refer [->UserCommandError]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [atom-of]]
            [schema.core :as s :refer [Any maybe]])
  (:import (clojure.lang IFn Keyword)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.application Application)))

(t/defrecord ^:private ApplicationMaster
  [application :- (atom-of Application "ApplicationAtom")
   status      :- AtomicBoolean
   exit-fn     :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! (:log-chan @application) this)
    (reset! application (comp/start @application))
    (let [f (future-catch-print
              (when (tty?)
                (with-console-input line
                  (let [cmd (parse-command-input line)]
                    (if (string? cmd)
                      (put! (:log-chan @application) (->UserCommandError cmd))
                      ;; TODO: Export to function
                      (case (:opcode cmd)
                        ;; Handle top-level commands directly
                        :app/help (.println System/err OPCODE-HELP)
                        ; :app/status
                        ; :app/reload
                        ; :app/restart
                        :app/quit (when (.get status) (comp/stop this))
                        ;; Convey everything else
                        (>!! (:cmd-chan @application) cmd)))))
                (when (.get status)
                  ;; User closed input stream
                  (comp/stop this))))]
      (assoc this :exit-fn
             #(do (.set status false)               ; Stop after current iteration
                  (close! (:cmd-chan @application)) ; Close outgoing channels
                  (close! (:log-chan @application))
                  (shutdown-future f 100)
                  (comp/stop @application)))))

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
        mbw (:mbwatch-config options DEFAULT-CONFIG-PATH)
        config (->Config options mbs mbw)
        cmd-chan (chan CHAN-SIZE)
        log-chan (chan CHAN-SIZE)]
    (strict-map->ApplicationMaster
      {:application (atom (->Application config cmd-chan log-chan))
       :status (AtomicBoolean. true)
       :exit-fn nil})))
