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
            [clojure.java.io :as io :refer [Coercions]]
            [clojure.java.shell :refer [sh]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.application :refer [->Application status-table]]
            [mbwatch.command :refer [CommandSchema OPCODE-HELP
                                     parse-command-input]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        sig-notify-all]]
            [mbwatch.config :refer [->Config DEFAULT-MBWATCHRC-PATH]]
            [mbwatch.config.mbsyncrc :refer [DEFAULT-MBSYNCRC-PATH]]
            [mbwatch.console :refer [console-reader print-console tty?
                                     with-reader-input]]
            [mbwatch.events :refer [->UserCommandFeedback]]
            [mbwatch.logging :refer [->LogItem log-with-timestamp!]]
            [mbwatch.logging.levels :refer [DEBUG]]
            [mbwatch.logging.protocols :refer [Loggable]]
            [mbwatch.posix :refer [create-dir remove-dir]]
            [mbwatch.types :as t :refer [VOID atom-of]]
            [schema.core :as s :refer [Any maybe]])
  (:import (clojure.lang IFn Keyword)
           (java.io File)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.application Application)))

(s/defn ^:private create-tempdir :- File
  []
  (create-dir (str "/tmp/mbwatch-" (System/getProperty "user.name")) 0700))

(s/defn ^:private create-control-pipe :- (maybe File)
  [dir :- Coercions]
  (let [path (io/file dir "control")]
    (when (zero? (:exit (sh "mkfifo" "-m" "0600" (str path))))
      path)))

(s/defn ^:private sig-exit :- VOID
  [status :- AtomicBoolean]
  (.set status false)
  (sig-notify-all status))

(declare process-command
         process-input)

(t/defrecord ApplicationMaster
  [application :- (atom-of Application "ApplicationAtom")
   status      :- AtomicBoolean
   exit-fn     :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! (:log-chan @application) this)
    (swap! application comp/start) ; START Application
    (let [tempdir (create-tempdir) ; CREATE tempdir
          c (future-catch-print
              (when (tty?)
                (try
                  (with-reader-input [line (console-reader)]
                    (process-input this line))
                  (catch InterruptedException _)) ; We are expecting this
                (when (.get status)
                  ;; User signaled EOF at the console (i.e. Control-D)
                  (print-console :err "Goodbye!")
                  (sig-exit status))))
          p (future-catch-print
              (when-some [path (create-control-pipe tempdir)]
                (try
                  (loop []
                    (with-open [pipe (io/reader path)]
                      (with-reader-input [line pipe]
                        (process-input this line)))
                    (recur))
                  (catch InterruptedException _))))]
      (assoc this :exit-fn
             #(do (.set status false)
                  (close! (:cmd-chan @application)) ; CLOSE cmd-chan
                  (close! (:log-chan @application)) ; CLOSE log-chan
                  (future-cancel c)                 ; Cancel the read on stdin
                  (future-cancel p)                 ; Cancel the read on the control pipe
                  (remove-dir tempdir)              ; DELETE tempdir
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

(s/defn ^:private process-input :- (maybe Boolean)
  [application-master :- ApplicationMaster
   line               :- String]
  (let [cmd (parse-command-input line)]
    (if (string? cmd)
      (put! (:log-chan @(:application application-master))
            (->UserCommandFeedback :parse-error cmd))
      (process-command application-master cmd))))

(s/defn ^:private process-command :- (maybe Boolean)
  [application-master :- ApplicationMaster
   command            :- CommandSchema]
  (case (:opcode command)
    ;; Handle top-level commands directly
    :app/help (do (print-console :err OPCODE-HELP)
                  true)
    :app/clear (do (let [{:keys [cache-atom log-chan]} @(:application application-master)]
                     (when cache-atom
                       (swap! cache-atom empty)
                       (put! log-chan (->UserCommandFeedback :app/clear))))
                   true)
    :app/status (do (print-console :err (status-table @(:application application-master)))
                    true)
    ; :app/reload
    ; :app/restart
    :app/quit (sig-exit (:status application-master))
    ;; Convey everything else
    (do
      (put! (-> application-master :application deref :cmd-chan) command)
      true)))
