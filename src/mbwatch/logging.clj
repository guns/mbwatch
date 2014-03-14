(ns mbwatch.logging
  (:require [clojure.core.async :refer [<!!]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.util :refer [class-name poison-chan thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [Int maybe]])
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

(def ^:private log-levels
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

  (log-level [_] NOTICE)

  (->log [this]
    (LogItem. NOTICE (DateTime.) (str this))))

(defprotocol IItemLogger
  (log [this ^LogItem log-item]))

(s/defrecord LoggingService
  [level      :- Int
   logger     :- IItemLogger
   log-chan   :- ReadPort
   state-chan :- (maybe ReadPort)]

  Lifecycle

  (start [this]
    (printf "↑ Starting %s [%s %s]\n" (class-name this) (get log-levels level) (class-name logger))
    (assoc this :state-chan
           (thread-loop []
             (with-chan-value [obj (<!! log-chan)]
               (when (<= (log-level obj) level)
                 (log logger (->log obj)))
               (recur)))))

  (stop [this]
    (printf "↓ Stopping %s [%s %s]\n" (class-name this) (get log-levels level) (class-name logger))
    (poison-chan log-chan state-chan)
    (dissoc this :state-chan)))
