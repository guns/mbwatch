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
  [id              :- Long
   channels->boxes :- {String [String]}]

  ICommand

  (command [_] :sync) ; This is perfunctory as we can dispatch on type

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s[%d] %s" (class-name this) id channels->boxes)]
      (LogItem. DEBUG (DateTime.) msg))))

(s/defn ->command :- ICommand
  [command :- (either {String [String]} Keyword (eq nil))]
  (if (map? command)
    (SyncCommand. (.getAndIncrement next-command-id) command)
    (Command. (or command :stop))))
