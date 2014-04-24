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
  (:require [clojure.core.async :refer [>!! put!]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.application :refer [->Application]]
            [mbwatch.command :refer [OPCODE-HELP parse-command-input]]
            [mbwatch.config]
            [mbwatch.console :refer [tty? with-console-input]]
            [mbwatch.events :refer [->UserCommandError]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [atom-of]]
            [schema.core :as s :refer [maybe]])
  (:import (clojure.lang IFn)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.application Application)
           (mbwatch.config Config)))

(t/defrecord ^:private ApplicationMaster
  [application :- (atom-of Application "ApplicationAtom")
   status      :- AtomicBoolean
   exit-fn     :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! (:log-chan @application) this)
    (reset! application (comp/start @application))
    (let [
          f (future
              (when (tty?)
                (with-console-input line
                  (let [{:keys [cmd-chan log-chan]} @application
                        cmd (parse-command-input line)]
                    (if (string? cmd)
                      (put! log-chan (->UserCommandError cmd))
                      (case (:opcode cmd)
                        :app/help (.println System/err OPCODE-HELP)
                        ; :app/status
                        ; :app/reload
                        ; :app/restart
                        :app/quit (when (.get status) (comp/stop this))
                        (>!! cmd-chan cmd)))))
                (when (.get status)
                  ;; User closed input stream
                  (.println System/err "Goodbye!")
                  (comp/stop this))))]
      (assoc this :exit-fn
             #(do (.set status false)
                  (future-cancel f)
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
  [config :- Config]
  (strict-map->ApplicationMaster
    {:application (atom (->Application config))
     :status (AtomicBoolean. true)
     :exit-fn nil}))
