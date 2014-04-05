(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [<!! >!! alts!! chan close! thread]]
            [mbwatch.util :refer [catch-print]]))

(def ^:const CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

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

(defn failsafe-pipe
  "Conveys elements from the from channel to the to channel. If the from
   channel closes, to is closed as well. If a transfer fails because the
   to channel is closed, the element is placed back on the from channel.
   Therefore, from should be a buffered channel to prevent hangs."
  [from to]
  (thread-loop []
    (let [v (<!! from)]
      (cond (nil? v) (close! to)
            (>!! to v) (recur)
            :else (>!! from v))))
  to)

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
