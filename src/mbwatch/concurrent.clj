(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [>!! alts!! chan thread]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s])
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
  ([monitor :- Object]
   (locking monitor (.wait monitor)))
  ([monitor :- Object
    timeout :- long]
   (when (pos? timeout)
     (locking monitor
       (.wait monitor timeout)))))

(s/defn sig-notify :- VOID
  [monitor :- Object]
  (locking monitor (.notify monitor)))

(s/defn sig-notify-all :- VOID
  [monitor :- Object]
  (locking monitor (.notifyAll monitor)))

(s/defn sig-wait-and-set-forward :- VOID
  "Wait for signals on monitor or wake up at alarm time, then reset the
   alarm `interval` milliseconds forward. This allows a periodic timer to be
   interrupted and reset on signal."
  [monitor  :- Object
   alarm    :- AtomicLong
   interval :- AtomicLong]
  (sig-wait monitor (- (.get alarm) (System/currentTimeMillis)))
  (.set alarm (+ (System/currentTimeMillis) (.get interval))))
