(ns mbwatch.mbsync.command
  (:require [mbwatch.logging :refer [->LogItem DEBUG Loggable]]
            [mbwatch.types :as t]
            [mbwatch.util :refer [class-name]]
            [schema.core :as s :refer [Int either enum]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong next-command-id
  "A synchronized counter for SyncCommands. There is no requirement to be
   either predictable or unpredictable, so this can be implemented as an
   incrementing global var."
  (AtomicLong. 1))

(defprotocol ICommand
  (command [this] "Returns a keyword representing an operation.")
  (timestamp [this] "Returns a DateTime"))

(t/defrecord ^:private Command
  [command   :- #{:term :stop}
   timestamp :- DateTime]

  ICommand

  (command [_] command)
  (timestamp [_] timestamp)

  Loggable

  (log-level [_] DEBUG)
  (->log [this] (->LogItem this (str (class-name this) ": " command))))

(t/defrecord ^:private SyncCommand
  [mbchan->mbox :- {String [String]}
   timestamp    :- DateTime
   id           :- Int]

  ICommand

  (command [_] :sync)
  (timestamp [_] timestamp)

  Loggable

  (log-level [_] DEBUG)
  (->log [this]
    (->LogItem this (format "%s: %d %s" (class-name this) id mbchan->mbox))))

(s/defn ->command :- ICommand
  [command :- (either {String [String]}
                      (enum :term :stop nil))]
  (if (map? command)
    (SyncCommand. command (DateTime.) (.getAndIncrement next-command-id))
    (Command. (or command :stop) (DateTime.))))
