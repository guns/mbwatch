(ns mbwatch.mbsync.command
  (:require [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.util :refer [class-name]]
            [schema.core :as s :refer [Int either enum]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong next-command-id
  "A synchronized counter for SyncCommands. There is no requirement to be
   either predictable or unpredictable, so we can store this as a global var."
  (AtomicLong. 1))

(defprotocol ICommand
  (command [this] "Returns a keyword representing an operation."))

(s/defrecord Command
  [command :- #{:term :stop}]

  ICommand

  (command [_] command)

  Loggable

  (log-level [_] DEBUG)
  (->log [this]
    (LogItem. DEBUG (DateTime.) (str (class-name this) ": " command))))

(s/defrecord SyncCommand
  [id           :- Int
   mbchan->mbox :- {String [String]}]

  ICommand

  (command [_] :sync)

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s: %d %s" (class-name this) id mbchan->mbox)]
      (LogItem. DEBUG (DateTime.) msg))))

(s/defn ->command :- ICommand
  [command :- (either {String [String]}
                      (enum :term :stop nil))]
  (if (map? command)
    (SyncCommand. (.getAndIncrement next-command-id) command)
    (Command. (or command :stop))))
