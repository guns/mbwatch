(ns mbwatch.types
  (:require [clojure.core :as cc]
            [schema.core :as s :refer [Schema both defschema eq maybe one
                                       pair pred validate]])
  (:import (clojure.lang Atom IPersistentMap Symbol))
  (:refer-clojure :exclude [defrecord]))

(defmacro defrecord
  "Same as clojure.core/defrecord or schema.core/defrecord, except that
   the ->name and map->name constructors are set to ^:private. The
   strict-map->name constructor remains public if the schema version is used,
   unless the name symbol is also marked ^:private."
  {:requires [#'cc/defrecord #'s/defrecord]}
  [name & body]
  (let [fields (first body)
        schema? (and (coll? fields) (some #{:-} fields))
        sym (fn [prefix] (symbol (str prefix name)))
        get-var (fn [prefix] `(resolve '~(sym prefix)))]
    `(do ~(if schema?
            `(s/defrecord ~name ~@body)
            `(cc/defrecord ~name ~@body))
         (alter-meta! ~(get-var "->") assoc :private true)
         (alter-meta! ~(get-var "map->") assoc :private true)
         ~(when (and schema? (-> name meta :private))
            `(alter-meta! ~(get-var "strict-map->") assoc :private true)))))

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

(defschema MBMap
  {String #{String}})

(defschema MBMapAtom
  (atom-of MBMap "MBMapAtom"))

(defschema MBTuple
  (pair String "mbchan"
        String "mbox"))

(defschema ConnectionMap
  {String {:status Boolean
           :pending-syncs (maybe #{String})}})

(defschema ConnectionMapAtom
  (atom-of ConnectionMap "ConnectionMapAtom"))

(defschema MapAtom
  (atom-of IPersistentMap "MapAtom"))
