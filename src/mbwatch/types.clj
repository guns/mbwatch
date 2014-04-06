(ns mbwatch.types
  (:require [clojure.core :as cc]
            [schema.core :as s :refer [both defschema eq pred]])
  (:refer-clojure :exclude [defrecord]))

(defmacro defrecord
  "Same as clojure.core/defrecord or schema.core/defrecord, except that the
   ->name, map->name, and strict-map->name constructors are made private. The
   strict-map->name constructor remains public if the schema version is used
   and the name symbol contains the :public flag in its metadata"
  {:requires [#'cc/defrecord #'s/defrecord]}
  [name & body]
  (let [fields (first body)
        schema? (and (coll? fields) (some #{:-} fields))
        get-var (fn [prefix] `(resolve '~(symbol (str prefix name))))]
    `(do ~(if schema?
            `(s/defrecord ~name ~@body)
            `(cc/defrecord ~name ~@body))
         (alter-meta! ~(get-var "->") assoc :private true)
         (alter-meta! ~(get-var "map->") assoc :private true)
         ~(when (and schema? (not (-> name meta :public)))
            `(alter-meta! ~(get-var "strict-map->") assoc :private true)))))

(defschema VOID
  (eq nil))

(defschema Word
  (pred #(and (string? %)
              (seq %)
              (not (re-find #"\s" %)))))

(defschema LowerCaseWord
  (both Word (pred #(not (re-find #"\p{Lu}" %)))))

(defschema FilteredLine
  (pred #(and (string? %)
              (not (re-seq #"\n|\A\s*#|\A\s*\z|\A\s|\s\z" %)))
        "single non-comment line with no surrounding whitespace"))

(defschema PortNumber
  (pred #(and (integer? %) (< 0 % 0x1000))))
