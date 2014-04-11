(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [thread]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s :refer [Int]])
  (:import (java.util.concurrent.atomic AtomicLong)))

(def ^:const CHAN-SIZE
  "4K ought to be enough for anybody."
  0x1000)

(defmacro future-catch-print
  {:requires [#'catch-print]}
  [& body]
  `(future
     (catch-print
       ~@body)))

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
   (locking lock
     (.wait lock)))
  ([lock    :- Object
    timeout :- long]
   (when (pos? timeout)
     (locking lock
       (.wait lock timeout)))))

(s/defn sig-notify :- VOID
  [lock :- Object]
  (locking lock
    (.notify lock)))

(s/defn sig-notify-all :- VOID
  [lock :- Object]
  (locking lock
    (.notifyAll lock)))

(s/defn sig-wait-alarm :- VOID
  "Wait for signals on alarm or wake up at alarm time. If the value of `alarm`
   has changed in the meantime, wait on alarm again until the new alarm time."
  [alarm :- AtomicLong]
  (loop [alarm-time (.get alarm)]
    (sig-wait alarm (- alarm-time (System/currentTimeMillis)))
    (let [alarm-time' (.get alarm)]
      (when-not (= alarm-time alarm-time')
        (recur alarm-time')))))

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
