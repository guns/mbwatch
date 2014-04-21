(ns mbwatch.time
  (:require [mbwatch.util :refer [istr=]]
            [schema.core :as s :refer [Int]])
  (:import (org.joda.time DateTime Duration Instant ReadableInstant)))

(s/defn human-duration :- String
  ([milliseconds :- Int]
   (let [seconds (Math/round (/ milliseconds 1000.0))
         h (quot seconds 3600)
         m (quot (rem seconds 3600) 60)
         s (rem seconds 60)
         xs (cond-> []
              (pos? h) (conj (format "%s hour%s"   h (if (= h 1) "" \s)))
              (pos? m) (conj (format "%s minute%s" m (if (= m 1) "" \s)))
              (pos? s) (conj (format "%s second%s" s (if (= s 1) "" \s))))]
     (case (count xs)
       0 "0 seconds"
       1 (first xs)
       2 (apply format "%s and %s" xs)
       3 (apply format "%s, %s, and %s" xs))))
  ([start :- ReadableInstant
    stop  :- ReadableInstant]
   (human-duration (.getMillis (Duration. start stop)))))

(s/defn dt->ms :- long
  [datetime :- DateTime]
  (.getMillis (Instant. datetime)))

(s/defn ^:private illegal-time-unit :- IllegalArgumentException
  [u :- String]
  (IllegalArgumentException.
    (str (pr-str u) " is an unknown time unit. Please use d, h, m, s, or ms.")))

(s/defn parse-ms :- long
  [s :- String]
  (reduce
    (fn [ms [_ n u]]
      (let [n (Double/parseDouble n)
            u (if (seq u) u "m")
            n (cond (istr= "d" u) (* n 1000 60 60 24)
                    (istr= "h" u) (* n 1000 60 60)
                    (istr= "m" u) (* n 1000 60)
                    (istr= "s" u) (* n 1000)
                    (istr= "ms" u) n
                    :else (throw (illegal-time-unit u)))]
        (+ ms (Math/round ^double n))))
    0 (re-seq #"(\d+(?:\.\d+)?)([^\d\s]*)" s)))
