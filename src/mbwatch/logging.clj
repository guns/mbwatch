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
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.concurrent :refer [thread-loop]]
            [mbwatch.types :as t :refer [VOID]]
            [mbwatch.util :refer [class-name schema-params]]
            [schema.core :as s :refer [Any Int maybe]])
  (:import (clojure.lang Associative IFn)
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

(def ^:private LOG-LEVELS
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
  {:pre [(nil? (:timestamp map))]}
  (assoc map :timestamp (DateTime.)))

(s/defn ^:private get-timestamp :- DateTime
  [obj :- Any]
  {:pre [(do (when (nil? (:timestamp obj))
               (.println System/err (str "No timestamp for " obj)))
             true)]}
  (or (:timestamp obj) (DateTime.)))

(s/defn log-with-timestamp! :- (maybe Boolean)
  "Assoc :timestamp into map and put! onto chan."
  [chan :- WritePort
   map  :- Associative]
  (put! chan (assoc-timestamp map)))

(s/defn ->LogItem :- LogItem
  "This is a convenience function for creating LogItems from objects
   without explicit :timestamp fields, intended to be used together with
   log-with-timestamp!"
  [loggable :- Object
   message  :- String]
  (LogItem. (log-level loggable) (get-timestamp loggable) message))

(defmacro defloggable
  "Defines a simple Loggable implementation with a constructor with implicit
   timestamp creation. The body is spliced into the log-level implementation
   and should return a LogItem message."
  {:requires [#'t/defrecord Loggable #'s/defn]}
  [name level fields & body]
  (let [ctor-name (with-meta (symbol (str "->" name)) (meta name))
        ctor-params (schema-params fields)]
    `(do
       (t/defrecord ~name
         [~'timestamp :- DateTime
          ~@fields]

         Loggable

         (~'log-level [_#] ~level)
         (~'log-item [this#] (new ~LogItem ~level ~'timestamp (do ~@body))))

       (s/defn ~ctor-name :- ~name
         "Constructor for a defloggable event."
         ~fields
         (new ~name (new ~DateTime) ~@ctor-params)))))

(extend-protocol Loggable
  ;; Fallback implementation
  Object

  (log-level [_] DEBUG)
  (log-item [this] (->LogItem this (str this))))

(defprotocol IItemLogger
  (log [this ^LogItem log-item]))

(s/defn ^:private log* :- VOID
  [logger    :- IItemLogger
   max-level :- Int
   loggable  :- Loggable]
  ;; Malkovich malkovich Malkovich!
  (when (<= (log-level loggable) max-level)
    (log logger (log-item loggable)))
  nil)

(t/defrecord ^:private LoggingService
  [level     :- Int
   logger    :- IItemLogger
   log-chan  :- ReadPort
   exit-fn   :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log* logger level (log-item (assoc-timestamp this)))
    (let [c (thread-loop []
              (when-some [loggable (<!! log-chan)]
                (log* logger level loggable)
                (recur)))]
      (assoc this :exit-fn
             #(<!! c) ; Process all loggables before exiting
             )))

  (stop [this]
    (exit-fn)
    (log* logger level (log-item (assoc-timestamp this)))
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s LoggingService [%s %s]"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            (get LOG-LEVELS level)
                            (class-name logger)))))

(s/defn ->LoggingService :- LoggingService
  [level    :- Int
   logger   :- IItemLogger
   log-chan :- ReadPort]
  (strict-map->LoggingService
    {:level level
     :logger logger
     :log-chan log-chan
     :exit-fn nil}))
