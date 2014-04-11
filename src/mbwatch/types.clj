(ns mbwatch.types
  (:require [clojure.core :as cc]
            [schema.core :as s :refer [both defschema either eq pred]])
  (:refer-clojure :exclude [defrecord]))

(defmacro defrecord
  "Same as clojure.core/defrecord or schema.core/defrecord, except that the
   ->name and map->name constructors are unmapped. The strict-map->name
   constructor remains if the schema version is used."
  {:requires [#'cc/defrecord #'s/defrecord]}
  [name & body]
  (let [fields (first body)
        schema? (and (coll? fields) (some #{:-} fields))
        sym (fn [prefix] (symbol (str prefix name)))
        get-var (fn [prefix] `(resolve '~(sym prefix)))]
    `(do ~(if schema?
            `(s/defrecord ~name ~@body)
            `(cc/defrecord ~name ~@body))
         (ns-unmap *ns* '~(sym "->"))
         (ns-unmap *ns* '~(sym "map->"))
         ~(when (and schema? (-> name meta :private))
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
  (pred #(and (integer? %) (< 0 % 0x10000))))

(defschema StringList
  (either [String] #{String}))
