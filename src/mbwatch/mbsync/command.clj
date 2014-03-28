(ns mbwatch.mbsync.command
  (:require [mbwatch.logging :refer [->log-item DEBUG Loggable]]
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
  (command [this] "Returns a keyword representing an operation.")
  (timestamp [this] "Returns a DateTime"))

(s/defrecord Command
  [command   :- #{:term :stop}
   timestamp :- DateTime]

  ICommand

  (command [_] command)
  (timestamp [_] timestamp)

  Loggable

  (log-level [_] DEBUG)
  (->log [this] (->log-item this (str (class-name this) ": " command))))

(s/defrecord SyncCommand
  [mbchan->mbox :- {String [String]}
   timestamp    :- DateTime
   id           :- Int]

  ICommand

  (command [_] :sync)
  (timestamp [_] timestamp)

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (->log-item this (format "%s: %d %s" (class-name this) id mbchan->mbox))))

(s/defn ->command :- ICommand
  [command :- (either {String [String]}
                      (enum :term :stop nil))]
  (if (map? command)
    (SyncCommand. command (DateTime.) (.getAndIncrement next-command-id))
    (Command. (or command :stop) (DateTime.))))
