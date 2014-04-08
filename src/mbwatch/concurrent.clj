(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [thread]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s :refer [Int]])
  (:import (java.util.concurrent.atomic AtomicLong)))

(def ^:const CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

(defmacro future-catch-print
  {:requires [#'catch-print]}
  [& body]
  `(future (catch-print ~@body)))

(defmacro future-loop
  {:requires [#'catch-print]}
  [bindings & body]
  `(future
     (catch-print
       (loop ~bindings
         ~@body))))

(defmacro thread-loop
  {:requires [#'thread #'catch-print]}
  [bindings & body]
  `(thread
     (catch-print
       (loop ~bindings
         ~@body))))

(s/defn sig-wait :- VOID
  ([lock :- Object]
   (locking lock (.wait lock)))
  ([lock    :- Object
    timeout :- long]
   (when (pos? timeout)
     (locking lock (.wait lock timeout)))))

(s/defn sig-notify :- VOID
  [lock :- Object]
  (locking lock (.notify lock)))

(s/defn sig-notify-all :- VOID
  [lock :- Object]
  (locking lock (.notifyAll lock)))

(s/defn sig-wait-and-set-forward :- VOID
  "Wait for signals on lock or wake up at alarm time, then reset the alarm
   `period` milliseconds forward. This change is unsynchronized. If the value
   of `alarm` has changed in the meantime, wait on lock again until the new
   alarm time.

   This is intended to implement a periodic timer that can be interrupted and
   reset on signal."
  [lock   :- Object
   period :- AtomicLong
   alarm  :- AtomicLong]
  (loop [alarm-ms (.get alarm)]
    (sig-wait lock (- alarm-ms (System/currentTimeMillis)))
    (let [alarm-ms' (.get alarm)]
      (when-not (= alarm-ms alarm-ms')
        (recur alarm-ms'))))
  (.set alarm (+ (System/currentTimeMillis) (.get period))))

(s/defn update-period-and-alarm! :- Boolean
  "Set the period to new-period-ms and reset the alarm accordingly if
   new-period is different from the current period value. Returns true
   if period and alarm were updated, false if not. These changes are
   unsynchronized."
  [new-period :- Int
   period     :- AtomicLong
   alarm      :- AtomicLong]
  (let [old-period (.get period)]
    (if (= new-period old-period)
      false
      (do (.set period new-period)
          (.set alarm (+ new-period (- (.get alarm) old-period)))
          true))))
