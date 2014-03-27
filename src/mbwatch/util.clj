(ns mbwatch.util
  (:require [clojure.core.async :refer [<!! >!! alts!! chan go thread]]
            [clojure.string :as string])
  (:import (java.net URLEncoder)
           (org.joda.time Instant ReadableInstant Seconds)))

(defn chomp
  "Like Ruby's String#chomp, remove either trailing newlines or a constant
   suffix."
  ([^String s]
   (string/trim-newline s))
  ([^String s ^String suffix]
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

(defn dequote
  "Dequote a double-quoted string."
  [^String s]
  (let [len (.length s)]
    (if (and (> len 1) (= \" (.charAt s 0) (.charAt s (dec len))))
      (string/replace (subs s 1 (dec len)) #"\\(.)" "$1")
      s)))

(defn shell-escape
  "Adapted from Ruby's Shellwords#shellescape()"
  [s]
  (if (empty? s)
    "''"
    (-> s
        (string/replace #"([^A-Za-z0-9_\-.,:\/@\n])" "\\\\$1")
        (string/replace #"\n" "'\n'"))))

(defn human-duration
  ([seconds]
   (let [h (quot seconds 3600)
         m (quot (rem seconds 3600) 60)
         s (rem seconds 60)
         xs (cond-> []
              (pos? h) (conj (format "%s hour%s"   h (if (= h 1) "" \s)))
              (pos? m) (conj (format "%s minute%s" m (if (= m 1) "" \s)))
              (pos? s) (conj (format "%s second%s" s (if (= s 1) "" \s))))]
     (case (count xs)
       0 "zero seconds"
       1 (first xs)
       2 (apply format "%s and %s" xs)
       3 (apply format "%s, %s, and %s" xs))))
  ([^ReadableInstant start ^ReadableInstant stop]
   (human-duration (.getSeconds (Seconds/secondsBetween start stop)))))

(defn class-name [obj]
  (.getSimpleName (class obj)))

(defn to-ms [datetime]
  (.getMillis (Instant. datetime)))

(defn url-for
  "Returns scheme://user@host:port with appropriate escaping."
  [scheme user host port]
  (str scheme "://" (URLEncoder/encode user) \@ (URLEncoder/encode host) \: port))

;;
;; Concurrency helpers
;;

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

;;
;; core.async helpers
;;

(defmacro thread-loop
  {:require [#'thread]}
  [bindings & body]
  `(thread (loop ~bindings ~@body)))

(defn poison-chan
  "Send a poison value on wr-chan and wait for a response on rd-chan."
  [wr-chan rd-chan]
  (>!! wr-chan ::poison)
  (<!! rd-chan))

(defmacro with-chan-value [[sym form] & body]
  `(let [~sym ~form]
     (when (and ~sym (not= ~sym ::poison))
       ~@body)))

(defmacro first-alt
  "Execute all expressions concurrently and return the value of the first to
   return, prioritized by the given order. All expressions are left to run to
   completion."
  {:require [#'go]}
  [& exprs]
  (let [n (count exprs)
        chans (gensym "chans")]
    `(let [~chans (repeatedly ~n ~chan)]
       ~@(mapv (fn [i] `(go (~'>! (nth ~chans ~i) ~(nth exprs i))))
               (range n))
       (first (~alts!! ~chans :priority true)))))
