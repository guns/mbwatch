(ns mbwatch.logging
  "The LoggingService component takes a Loggable object from a channel,
   converts it to a LogItem, then logs it with its IItemLogger implementation.
   The incoming Loggable object is simply discarded if its log-level exceeds
   that of the LoggingService.

                       ┌────────────────┐
      ─── Loggable ──▶ │ LoggingService │
                       └────────────────┘
  "
  (:require [clojure.core.async :refer [<!! close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.concurrent :refer [thread-loop]]
            [mbwatch.types :as t]
            [mbwatch.util :refer [class-name]]
            [schema.core :as s :refer [Any Int maybe protocol]])
  (:import (clojure.lang Associative)
           (org.joda.time DateTime)))

;; From linux/kern_levels.h
(def ^:const EMERG   0) ; /* system is unusable */
(def ^:const ALERT   1) ; /* action must be taken immediately */
(def ^:const CRIT    2) ; /* critical conditions */
(def ^:const ERR     3) ; /* error conditions */
(def ^:const WARNING 4) ; /* warning conditions */
(def ^:const NOTICE  5) ; /* normal but significant condition */
(def ^:const INFO    6) ; /* informational */
(def ^:const DEBUG   7) ; /* debug-level messages */

(def ^:private ^:const LOG-LEVELS
  (mapv str '[EMERG ALERT CRIT ERR WARNING NOTICE INFO DEBUG]))

(defprotocol Loggable
  (log-level [this] "Returns this object's logging level")
  (log-item [this] "Returns a new LogItem object"))

(t/defrecord ^:private LogItem
  [level     :- Int
   timestamp :- DateTime
   message   :- String]

  Loggable

  (log-level [_] level)
  (log-item [this] this))

(s/defn ^:private assoc-timestamp :- {:timestamp DateTime Any Any}
  [map :- Associative]
  (assoc map :timestamp (DateTime.)))

(s/defn ^:private get-timestamp :- DateTime
  [obj :- Any]
  (or (:timestamp obj) (DateTime.)))

(s/defn log! :- (maybe Boolean)
  "Assoc :timestamp into map and put! onto chan."
  [chan :- WritePort
   map  :- Associative]
  (put! chan (assoc-timestamp map)))

(s/defn ->LogItem :- LogItem
  "Create a LogItem from a Loggable, assuming that the timestamp can be found
   in the :timestamp field of the Loggable."
  [loggable :- Loggable
   message  :- String]
  (LogItem. (log-level loggable) (get-timestamp loggable) message))

(extend-protocol Loggable

  ;; Fallback implementation
  Object

  (log-level [_] DEBUG)
  (log-item [this] (->LogItem this (str this))))

(defprotocol IItemLogger
  (log [this ^LogItem log-item]))

(t/defrecord ^:private LoggingService
  [level     :- Int
   logger    :- IItemLogger
   log-chan  :- ReadPort
   exit-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (assoc this :exit-chan
           (thread-loop []
             (when-some [obj (<!! log-chan)]
               (when (<= (log-level obj) level)
                 (log logger (log-item obj)))
               (recur)))))

  (stop [this]
    ;; Exit gracefully
    (log! log-chan this)
    (close! log-chan)
    (<!! exit-chan)
    (dissoc this :exit-chan))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s LoggingService [%s %s]"
                            (if exit-chan "↓ Stopping" "↑ Starting")
                            (get LOG-LEVELS level)
                            (class-name logger)))))

(s/defn ->LoggingService :- LoggingService
  [level logger log-chan]
  (strict-map->LoggingService
    {:level level
     :logger logger
     :log-chan log-chan
     :exit-chan nil}))
