(ns mbwatch.util
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string]
            [mbwatch.types :refer [MbTuple NotifyMap StringList SyncRequest]]
            [schema.core :as s :refer [Int pair pred]])
  (:import (clojure.lang Keyword Symbol)
           (java.net URLEncoder)
           (org.joda.time DateTime Duration Instant ReadableInstant)))

(s/defn parse-mbargs :- SyncRequest
  "Parse mbsync string arguments. The mbox value of an mbchan with no box
   arguments is set to [\"INBOX\"]."
  [argv :- [String]]
  (reduce
    (fn [m arg]
      (let [[[_ chan bs]] (re-seq #"\A([^:]+)(?=:(.*))?" arg)]
        (assoc m chan (if (and bs (seq bs))
                        (string/split bs #",")
                        ["INBOX"]))))
    {} argv))

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

(s/defn dt->ms :- long
  [datetime :- DateTime]
  (.getMillis (Instant. datetime)))

(s/defn class-name :- String
  [obj :- Object]
  (.getSimpleName (class obj)))

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

(s/defn map-mbtuples :- #{MbTuple}
  [notify-map :- NotifyMap]
  (reduce-kv
    (fn [v mbchan mboxes]
      (reduce
        (fn [v mbox]
          (conj v [mbchan mbox]))
        v mboxes))
    #{} notify-map))

(s/defn reduce-mbtuples :- NotifyMap
  [mbtuples :- #{MbTuple}]
  (reduce
    (fn [m [mbchan mbox]]
      (update-in m [mbchan] #(conj (or % #{}) mbox)))
    {} mbtuples))

(s/defn ^:private notify-map-diff* :- (pair NotifyMap "removals" NotifyMap "additions")
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

(s/defn notify-map-diff :- (pair #{MbTuple} "removals" #{MbTuple} "additions")
  [nm₁ :- NotifyMap
   nm₂ :- NotifyMap]
  (->> (notify-map-diff* nm₁ nm₂)
       (mapv map-mbtuples)))

(s/defn notify-map-disj :- NotifyMap
  [nm₁ :- NotifyMap
   nm₂ :- NotifyMap]
  (reduce-kv
    (fn [m mbchan mboxes]
      (let [m (update-in m [mbchan] difference mboxes)]
        (if (seq (m mbchan))
          m
          (dissoc m mbchan))))
    nm₁ nm₂))

(defmacro catch-print [& body]
  `(try
     ~@body
     (catch Throwable e#
       (.println System/err e#))))
