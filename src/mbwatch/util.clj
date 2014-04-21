(ns mbwatch.util
  (:require [clojure.string :as string]
            [schema.core :as s :refer [Int]])
  (:import (clojure.lang Keyword Symbol)
           (java.net URLEncoder)
           (org.joda.time DateTime Duration Instant ReadableInstant)))

(defmacro catch-print [& body]
  `(try
     ~@body
     (catch Throwable e#
       (.println System/err e#))))

(s/defn schema-params :- [Symbol]
  "Remove `:- Schema` information from a parameter list."
  [params :- [Object]]
  (loop [v [] params params]
    (if-some [p (first params)]
      (if (= (second params) :-)
        (recur (conj v p) (drop 3 params))
        (recur (conj v p) (rest params)))
      v)))

(s/defn parse-kv-string :- {Keyword String}
  "Simple key = value parser. Like ini, but without hierarchy, multiline
   values, or very many features at all."
  [s :- String]
  (let [lines (remove (partial re-find #"\A\s*[#;]|\A\s*\z")
                      (string/split s #"\n"))]
    (reduce
      (fn [m l]
        (let [[k v] (mapv string/trim (string/split l #"=" 2))]
          (when (or (nil? k) (nil? v))
            (throw (RuntimeException. (str "Malformed config line:\n" l))))
          (assoc m (keyword k) v)))
      {} lines)))

(s/defn istr= :- Boolean
  [s₁ :- String
   s₂ :- String]
  (zero? (.compareToIgnoreCase s₁ s₂)))

(s/defn chomp :- String
  "Like Ruby's String#chomp, remove trailing newlines or a constant suffix."
  ([s :- String]
   (string/trim-newline s))
  ([s      :- String
    suffix :- String]
   (let [len (.length s)
         slen (.length suffix)]
     (cond (zero? len) ""
           (zero? slen) s
           (< len slen) s
           :else (loop [offset 1]
                   (cond (> offset slen)
                         (subs s 0 (- len slen))

                         (= (.charAt s (- len offset))
                            (.charAt suffix (- slen offset)))
                         (recur (inc offset))

                         :else s))))))

(s/defn dequote :- String
  "Dequote a double-quoted string."
  [s :- String]
  (let [len (.length s)]
    (if (and (> len 1) (= \" (.charAt s 0) (.charAt s (dec len))))
      (string/replace (subs s 1 (dec len)) #"\\(.)" "$1")
      s)))

(s/defn class-name :- String
  [obj :- Object]
  (.getSimpleName (class obj)))

(s/defn url-for :- String
  "Returns scheme://user@host:port with appropriate escaping."
  [scheme :- Object
   user   :- String
   host   :- String
   port   :- Object]
  (str scheme "://"
       (URLEncoder/encode user "UTF-8") \@
       (URLEncoder/encode host "UTF-8") \: port))

(s/defn zero-or-min :- long
  "{n ∈ ℕ : n = 0, n ≥ min}"
  [n   :- long
   min :- long]
  (if (pos? n)
    (max n min)
    0))

;;
;; Time functions
;;

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
            n (cond (istr= "d" u) (* n 24 60 60 1000)
                    (istr= "h" u) (* n 60 60 1000)
                    (istr= "m" u) (* n 60 1000)
                    (istr= "s" u) (* n 1000)
                    (istr= "ms" u) n
                    :else (throw (illegal-time-unit u)))]
        (+ ms (Math/round ^double n))))
    0 (re-seq #"(\d+(?:\.\d+)?)([^\d\s]*)" s)))
