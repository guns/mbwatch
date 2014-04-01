(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [<!! >!! alts!! chan thread]]))

(def ^:const POISON ::poison)

(defmacro thread-loop
  {:require [#'thread]}
  [bindings & body]
  `(thread
     (try
       (loop ~bindings
         ~@body)
       (catch Throwable e#
         (.println System/err e#)))))

(defmacro with-chan-value [[sym form] & body]
  `(let [~sym ~form]
     (when (and ~sym (not= ~sym ~POISON))
       ~@body)))

(defn poison-chan
  "Send a poison value on wr-chan and wait for a response on rd-chan."
  [wr-chan rd-chan]
  (>!! wr-chan POISON)
  (<!! rd-chan))

(defmacro first-alt
  "Execute all expressions concurrently and return the value of the first to
   return, prioritized by the given order. All expressions are left to run to
   completion."
  {:require [#'thread]}
  [& exprs]
  (let [n (count exprs)
        chans (gensym "chans")]
    `(let [~chans (repeatedly ~n ~chan)]
       ~@(mapv (fn [i] `(thread (~>!! (nth ~chans ~i) ~(nth exprs i))))
               (range n))
       (first (~alts!! ~chans :priority true)))))

(defn sig-wait
  ([^Object monitor]
   (locking monitor (.wait monitor)))
  ([^Object monitor ^long timeout]
   (when (pos? timeout)
     (locking monitor
       (.wait monitor timeout)))))

(defn sig-notify [^Object monitor]
  (locking monitor (.notify monitor)))

(defn sig-notify-all [^Object monitor]
  (locking monitor (.notifyAll monitor)))
