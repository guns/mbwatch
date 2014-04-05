(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [>!! alts!! chan thread]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s]))

(def ^:const CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

(defmacro future-catch-print
  {:requires [#'catch-print]}
  [& body]
  `(future (catch-print ~@body)))

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

(s/defn sig-wait :- Object
  ([monitor :- Object]
   (locking monitor (.wait monitor))
   monitor)
  ([monitor :- Object
    timeout :- long]
   (when (pos? timeout)
     (locking monitor
       (.wait monitor timeout)))
   monitor))

(s/defn sig-notify :- Object
  [monitor :- Object]
  (locking monitor (.notify monitor))
  monitor)

(s/defn sig-notify-all :- Object
  [monitor :- Object]
  (locking monitor (.notifyAll monitor))
  monitor)
