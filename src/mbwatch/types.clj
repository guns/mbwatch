(ns mbwatch.types
  (:require [schema.core :as s :refer [defschema eq]])
  (:import (org.joda.time DateTime)))

(defschema VOID
  (eq nil))

;; From linux/kern_levels.h
(def ^:const EMERG   0) ; /* system is unusable */
(def ^:const ALERT   1) ; /* action must be taken immediately */
(def ^:const CRIT    2) ; /* critical conditions */
(def ^:const ERR     3) ; /* error conditions */
(def ^:const WARNING 4) ; /* warning conditions */
(def ^:const NOTICE  5) ; /* normal but significant condition */
(def ^:const INFO    6) ; /* informational */
(def ^:const DEBUG   7) ; /* debug-level messages */

(s/defrecord LogItem
  [level     :- s/Int
   timestamp :- DateTime
   message   :- String])

(defprotocol Loggable
  (->log [this] "Returns a new LogItem object"))
