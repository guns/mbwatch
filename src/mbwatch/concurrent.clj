(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [>!! alts!! chan thread]]
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

(defmacro first-alt
  "Execute all expressions concurrently and return the value of the first to
   return, prioritized by the given order. All expressions are left to run to
   completion."
  {:requires [#'thread]}
  [& exprs]
  (let [n (count exprs)
        chans (gensym "chans")]
    `(let [~chans (repeatedly ~n ~chan)]
       ~@(mapv (fn [i] `(thread (~>!! (nth ~chans ~i) ~(nth exprs i))))
               (range n))
       (first (~alts!! ~chans :priority true)))))

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
   `period` milliseconds forward. If the value of `alarm` has changed in the
   meantime, wait on lock again until the new alarm time.

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

(s/defn reset-period-and-alarm :- VOID
  "Set the period to new-period-ms and reset the alarm accordingly."
  [new-period-ms :- Int
   period        :- AtomicLong
   alarm         :- AtomicLong]
  (let [prev-alarm (.get alarm)]
    (.set alarm (+ new-period-ms (- prev-alarm (.get period))))
    (.set period new-period-ms)))
