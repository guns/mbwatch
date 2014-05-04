(ns mbwatch.types
  (:require [clojure.core :as cc]
            [immutable-int-map]
            [schema.core :as s :refer [Int Schema both defschema either eq
                                       maybe one pair pred validate]])
  (:import (clojure.lang Atom IPersistentMap Symbol)
           (immutable_int_map IRadix)
           (java.util.regex Pattern))
  (:refer-clojure :exclude [defrecord]))

(defmacro defrecord
  "Same as clojure.core/defrecord or schema.core/defrecord, except that the
   ->name, map->name, and strict-map->name constructors are set to ^:private."
  {:requires [#'cc/defrecord #'s/defrecord]}
  [name & body]
  (let [fields (first body)
        get-var (fn [prefix] `(resolve '~(symbol (str prefix name))))]
    `(do ~(if (and (coll? fields) (some #{:-} fields))
            `(s/defrecord ~name ~@body)
            `(cc/defrecord ~name ~@body))
         (alter-meta! ~(get-var "->") assoc :private true)
         (alter-meta! ~(get-var "map->") assoc :private true)
         (alter-meta! ~(get-var "strict-map->") assoc :private true))))

(defmacro tuple [& schemas]
  `[~@(mapv (fn [s] `(~one ~s ~(str s))) schemas)])

(s/defn atom-of :- Schema
  [inner-schema :- Schema
   desc         :- String]
  (pred #(and (instance? Atom %)
              (validate inner-schema @%))
        desc))

(s/defn schema-params :- [Symbol]
  "Remove `:- Schema` information from a parameter list."
  [params :- [Object]]
  (loop [v [] params params]
    (if-some [p (first params)]
      (if (= (second params) :-)
        (recur (conj v p) (drop 3 params))
        (recur (conj v p) (rest params)))
      v)))

(defschema VOID
  (eq nil))

(defschema Word
  (pred #(and (string? %)
              (seq %)
              (not (re-find #"\s" %)))
        "Word"))

(defschema LowerCaseWord
  (both Word (pred #(not (re-find #"\p{Lu}" %)) "LowerCase")))

(defschema FilteredLine
  (pred #(and (string? %)
              (not (re-seq #"\n|\A\s*#|\A\s*\z|\A\s|\s\z" %)))
        "single non-comment line with no surrounding whitespace"))

(defschema PortNumber
  (pred #(and (integer? %) (< 0 % 0x10000)) "PortNumber"))

(defschema IMAPCredential
  {:host String
   :port PortNumber
   :user String
   :pass (either String ; PassCmd
                 bytes) ; Plaintext password, stored as a byte-array
   :cert (maybe String)
   :ssl? Boolean})

(defschema Maildirstore
  {:inbox   FilteredLine
   :path    FilteredLine
   :flatten (maybe FilteredLine)})

(defschema MBMap
  {String #{String}})

(defschema MBMapAtom
  (atom-of MBMap "MBMapAtom"))

(defschema MBMap+
  (both MBMap
        (pred #(every? seq (vals %)) "mboxes present?")))

(defschema MBMap+Atom
  (atom-of MBMap+ "MBMap+Atom"))

(defschema MBTuple
  (pair String "mbchan"
        String "mbox"))

(defschema ConnectionMap
  {String {:status Boolean
           :pending-syncs (maybe #{String})}})

(defschema ConnectionMapAtom
  (atom-of ConnectionMap "ConnectionMapAtom"))

(defschema NotifyPatterns
  "mbchan {header-name #{Pattern}}. header-name of nil means search the body."
  {String {(maybe String) #{Pattern}}})

(defschema MapAtom
  (atom-of IPersistentMap "MapAtom"))

(defschema PosInt
  (both Int (pred #(<= 0 %) "n ≥ 0")))

(defschema TrieNode
  (both IRadix {PosInt IRadix}))
