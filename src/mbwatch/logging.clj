(ns mbwatch.logging
  "The LoggingService component takes a Loggable object from a channel,
   converts it to a LogItem, then logs it with its IItemLogger implementation.
   The incoming Loggable object is simply discarded if its log-level exceeds
   that of the LoggingService.

                       ┌────────────────┐
      ─── Loggable ──▶ │ LoggingService │
                       └────────────────┘
  "
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.util :refer [class-name poison-chan thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [Int maybe protocol]])
  (:import (org.joda.time DateTime)))

;; From linux/kern_levels.h
(def ^:const EMERG   0) ; /* system is unusable */
(def ^:const ALERT   1) ; /* action must be taken immediately */
(def ^:const CRIT    2) ; /* critical conditions */
(def ^:const ERR     3) ; /* error conditions */
(def ^:const WARNING 4) ; /* warning conditions */
(def ^:const NOTICE  5) ; /* normal but significant condition */
(def ^:const INFO    6) ; /* informational */
(def ^:const DEBUG   7) ; /* debug-level messages */

(def ^:private LOG-LEVELS
  (mapv str '[EMERG ALERT CRIT ERR WARNING NOTICE INFO DEBUG]))

(defprotocol Loggable
  (log-level [this] "Returns this object's logging level")
  (->log [this] "Returns a new LogItem object"))

(s/defrecord LogItem
  [level     :- Int
   timestamp :- DateTime
   message   :- String]

  Loggable

  (log-level [_] level)
  (->log [this] this))

(extend-protocol Loggable
  ;; Fallback implementation
  Object

  (log-level [_] DEBUG)
  (->log [this]
    (LogItem. DEBUG (DateTime.) (str this))))

(defprotocol IItemLogger
  (log [this ^LogItem log-item]))

(s/defrecord LoggingService
  [level      :- Int
   logger     :- IItemLogger
   log-chan   :- ReadPort
   state-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (put! log-chan this)
    (assoc this :state-chan
           (thread-loop []
             (with-chan-value [obj (<!! log-chan)]
               (when (<= (log-level obj) level)
                 (log logger (->log obj)))
               (recur)))))

  (stop [this]
    (put! log-chan this)
    (poison-chan log-chan state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s %s [%s %s]"
                      (if state-chan "↓ Stopping" "↑ Starting")
                      (class-name this)
                      (get LOG-LEVELS level)
                      (class-name logger))]
      (LogItem. DEBUG (DateTime.) msg))))
