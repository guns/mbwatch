(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [<!! >!! alts!! chan thread]]
            [mbwatch.util :refer [catch-print]]))

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
