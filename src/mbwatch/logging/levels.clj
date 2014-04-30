(ns mbwatch.logging.levels
  (:require [clojure.set :refer [map-invert]]))

;; From linux/kern_levels.h
(def ^:const EMERG   0) ; /* system is unusable */
(def ^:const ALERT   1) ; /* action must be taken immediately */
(def ^:const CRIT    2) ; /* critical conditions */
(def ^:const ERR     3) ; /* error conditions */
(def ^:const WARNING 4) ; /* warning conditions */
(def ^:const NOTICE  5) ; /* normal but significant condition */
(def ^:const INFO    6) ; /* informational */
(def ^:const DEBUG   7) ; /* debug-level messages */

(def NAME->LEVEL
  {'EMERG   EMERG
   'ALERT   ALERT
   'CRIT    CRIT
   'ERR     ERR
   'WARNING WARNING
   'NOTICE  NOTICE
   'INFO    INFO
   'DEBUG   DEBUG})

(def LEVEL->NAME
  (map-invert NAME->LEVEL))
