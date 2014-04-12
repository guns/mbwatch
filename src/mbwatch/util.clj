(ns mbwatch.util
  (:require [clojure.string :as string]
            [mbwatch.types :refer [StringList]]
            [schema.core :as s :refer [Int]])
  (:import (clojure.lang Symbol)
           (java.net URLEncoder)
           (org.joda.time DateTime Duration Instant ReadableInstant)))

(s/defn join-mbargs :- String
  [mbchan :- String
   mboxes :- StringList]
  (if (seq mboxes)
    (str mbchan \: (string/join \, mboxes))
    (str mbchan)))

(s/defn join-sync-request :- String
  [sync-request :- {String StringList}]
  (->> sync-request
       sort
       (mapv (fn [[mbchan mboxes]] (join-mbargs mbchan (sort mboxes))))
       (string/join \space)))

(s/defn schema-params :- [Symbol]
  "Remove `:- Type` information from a parameter list."
  [params :- [Object]]
  (loop [v [] params params]
    (if-some [p (first params)]
      (if (= (second params) :-)
        (recur (conj v p) (drop 3 params))
        (recur (conj v p) (rest params)))
      v)))

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

(s/defn shell-escape :- String
  "Adapted from Ruby's Shellwords#shellescape()"
  [s :- String]
  (if (empty? s)
    "''"
    (-> s
        (string/replace #"([^A-Za-z0-9_\-.,:\/@\n])" "\\\\$1")
        (string/replace #"\n" "'\n'"))))

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

(s/defn class-name :- String
  [obj :- Object]
  (.getSimpleName (class obj)))

(s/defn to-ms :- Long
  [datetime :- DateTime]
  (.getMillis (Instant. datetime)))

(s/defn url-for :- String
  "Returns scheme://user@host:port with appropriate escaping."
  [scheme :- Object
   user   :- String
   host   :- String
   port   :- Object]
  (str scheme "://"
       (URLEncoder/encode user "UTF-8") \@
       (URLEncoder/encode host "UTF-8") \: port))

(defmacro catch-print [& body]
  `(try
     ~@body
     (catch Throwable e#
       (.println System/err e#))))
