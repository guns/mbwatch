(ns mbwatch.util
  (:require [clojure.string :as string]
            [schema.core :as s])
  (:import (clojure.lang Keyword)
           (java.net URLEncoder)))

(defmacro catch-print [& body]
  `(try
     ~@body
     (catch InterruptedException e#
       (.println System/err e#))
     (catch Throwable e#
       (.printStackTrace e#))))

(defmacro when-seq [[sym form] & body]
  `(let [x# ~form]
     (when (seq x#)
       (let [~sym x#]
         ~@body))))

(defmacro case+
  "The case macro, except literal vector test constants are unpacked as
   multiple constants to the same result expr."
  [e & clauses]
  `(case ~e
     ~@(loop [[t expr & more] clauses cs []]
         (cond (nil? t) cs
               (nil? expr) (conj cs t)
               (vector? t) (recur more (into cs (interleave t (repeat expr))))
               :else (recur more (conj cs t expr))))))

(s/defn make-table :- String
  [headings :- [String]
   rows     :- [[String]]]
  (let [widths (->> rows
                    (cons headings)
                    (apply mapv vector)
                    (mapv #(apply max (mapv count %))))
        fmt (->> widths
                 (mapv #(format "%%-%ds" %))
                 (string/join " | "))
        sep (->> widths
                 (mapv #(apply str (repeat % \-)))
                 (string/join "-+-"))]
    (->> (concat [(apply format fmt headings)
                  sep]
                 (mapv #(apply format fmt %) rows))
         (string/join \newline))))

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
