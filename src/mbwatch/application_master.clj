(ns mbwatch.application-master
  "
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
            [mbwatch.command :refer [parse-command-input]]
            [mbwatch.config :refer [->Config]]
            [mbwatch.console :refer [tty? with-console-input]]
            [mbwatch.core :refer [->Application]]
            [mbwatch.events :refer [->UserCommandError]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [atom-of]]
            [schema.core :as s :refer [Any maybe]])
  (:import (clojure.lang IFn Keyword)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.core Application)))

(t/defrecord ^:private ApplicationMaster
  [application :- (atom-of Application "ApplicationAtom")
   status      :- AtomicBoolean
   exit-fn     :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! (:log-chan @application) this)
    (reset! application (comp/start @application))
    (let [{:keys [cmd-chan log-chan]} @application
          f (future
              (when (tty?)
                (with-console-input line
                  (let [cmd (parse-command-input line)]
                    (if (string? cmd)
                      (put! log-chan (->UserCommandError cmd))
                      (case (:opcode cmd)
                        ; :app/help
                        ; :app/status
                        ; :app/reload
                        ; :app/restart
                        ; :app/quit
                        (>!! cmd-chan cmd))))))
              (when (.get status)
                (comp/stop this)))]
      (assoc this :exit-fn
             #(do (comp/stop @application)))))

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
  [options :- {Keyword Any}]
  (let [{:keys [config mbwatch-config]} options]
    (strict-map->ApplicationMaster
      {:application (atom (->Application (->Config options config mbwatch-config)))
       :status (AtomicBoolean. true)
       :exit-fn nil})))
