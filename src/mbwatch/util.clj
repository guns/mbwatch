(ns mbwatch.util
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string]
            [mbwatch.types :refer [NotifyMap StringList SyncRequest]]
            [schema.core :as s :refer [Int pair pred]])
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
  [sync-req :- SyncRequest]
  (->> sync-req
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

(s/defn zero-or-min :- (pred #(>= % 0))
  "{n ∈ ℕ : n = 0, n ≥ min}"
  [n   :- long
   min :- long]
  (if (pos? n)
    (max n min)
    0))

(s/defn url-for :- String
  "Returns scheme://user@host:port with appropriate escaping."
  [scheme :- Object
   user   :- String
   host   :- String
   port   :- Object]
  (str scheme "://"
       (URLEncoder/encode user "UTF-8") \@
       (URLEncoder/encode host "UTF-8") \: port))

(s/defn ^:private notify-map-entries :- #{(pair String "mbchan" String "mbox")}
  [notify-map :- NotifyMap]
  (reduce-kv
    (fn [v mbchan mboxes]
      (reduce
        (fn [v mbox]
          (conj v [mbchan mbox]))
        v mboxes))
    #{} notify-map))

(s/defn ^:private notify-map-diff* :- (pair NotifyMap "removals"
                                  NotifyMap "additions")
  [nm₁ :- NotifyMap
   nm₂ :- NotifyMap]
  (reduce
    (fn [[rem add] mbchan]
      (let [s₁ (or (nm₁ mbchan) #{})
            s₂ (or (nm₂ mbchan) #{})
            Δ- (difference s₁ s₂)
            Δ+ (difference s₂ s₁)]
        [(cond-> rem (seq Δ-) (assoc mbchan Δ-))
         (cond-> add (seq Δ+) (assoc mbchan Δ+))]))
    [{} {}] (distinct (mapcat keys [nm₁ nm₂]))))

(s/defn notify-map-diff :- (pair #{(pair String "mbchan" String "mbox")} "removals"
                                 #{(pair String "mbchan" String "mbox")} "additions")
  [nm₁ :- NotifyMap
   nm₂ :- NotifyMap]
  (->> (notify-map-diff* nm₁ nm₂)
       (mapv notify-map-entries)))

(defmacro catch-print [& body]
  `(try
     ~@body
     (catch Throwable e#
       (.println System/err e#))))
