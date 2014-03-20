(ns mbwatch.mbsync.command
  (:require [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.util :refer [class-name]]
            [schema.core :as s :refer [either eq]])
  (:import (clojure.lang Keyword)
           (java.util.concurrent.atomic AtomicLong)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong next-command-id
  (AtomicLong.))

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
  [id           :- Long
   mbchan->mbox :- {String [String]}]

  ICommand

  (command [_] :sync)

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s: %s (%d)" (class-name this) mbchan->mbox id)]
      (LogItem. DEBUG (DateTime.) msg))))

(s/defn ->command :- ICommand
  [command :- (either {String [String]} Keyword (eq nil))]
  (if (map? command)
    (SyncCommand. (.getAndIncrement next-command-id) command)
    (Command. (or command :stop))))
