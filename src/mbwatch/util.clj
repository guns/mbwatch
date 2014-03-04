(ns mbwatch.util
  (:require [clojure.string :as string]))

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
