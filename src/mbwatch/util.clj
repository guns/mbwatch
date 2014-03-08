(ns mbwatch.util
  (:require [clojure.core.async :refer [<!! >!! thread]]
            [clojure.string :as string]))

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

(defmacro thread-loop
  {:require [#'thread]}
  [bindings & body]
  `(thread (loop ~bindings ~@body)))

(def ^:private poison ::poison)

(defn poison-chan
  "Send a poison value on wr-chan and wait for a response on rd-chan."
  [wr-chan rd-chan]
  (>!! wr-chan poison)
  (<!! rd-chan))

(defmacro with-chan-value [[sym form] & body]
  `(let [~sym ~form]
     (when (and ~sym (not= ~sym ~poison))
       ~@body)))
