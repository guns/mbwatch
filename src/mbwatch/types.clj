(ns mbwatch.types
  (:require [clojure.core.async.impl.protocols :refer [Buffer]]
            [clojure.string :as string]
            [schema.core :as s :refer [Int defschema eq]])
  (:import (clojure.lang Counted)
           (java.util LinkedList)))

(defschema VOID
  (eq nil))

(deftype UniqueBuffer [^LinkedList buf ^long n]
  Buffer

  (full? [this]
    (= (.size buf) n))

  (remove! [this]
    (.removeLast buf))

  (add! [this item]
    (assert (< (.size buf) n) "Can't add to a full buffer")
    (when-not (.contains buf item)
      (.addFirst buf item)))

  Counted

  (count [this]
    (.size buf)))

(s/defn ->UniqueBuffer :- UniqueBuffer
  [n :- Int]
  (UniqueBuffer. (LinkedList.) n))

(s/defrecord Passwd
  [name   :- String
   passwd :- String
   uid    :- Int
   gid    :- Int
   gecos  :- String
   dir    :- String
   shell  :- String])

(s/defn parse-passwd :- [Passwd]
  [s :- String]
  (mapv (fn [line]
          (-> (string/split line #"(?<!\\):" 7)
              (as-> ls (mapv #(string/replace % #"\\(.)" "$1") ls))
              (update-in [2] #(Integer/parseInt %))
              (update-in [3] #(Integer/parseInt %))
              ((fn [[a b c d e f g]] (Passwd. a b c d e f g)))))
        (string/split-lines s)))

(s/defn expand-user-path :- String
  [pwents :- [Passwd]
   path   :- String]
  (if (= (first path) \~)
    (if (= (second path) \/)
      (str (System/getProperty "user.home") (subs path 1))
      (let [i (.indexOf path "/")
            user (subs path 1 i)
            dir (:dir (first (filter #(= (:name %) user) pwents)))]
        (if dir
          (str dir (subs path i))
          path)))
    path))
