(ns mbwatch.types
  (:require [clojure.core :as cc]
            [schema.core :as s :refer [defschema eq]])
  (:refer-clojure :exclude [defrecord]))

(defprotocol ICommand
  (command [this] "Returns a keyword representing an operation.")
  (timestamp [this] "Returns a DateTime"))

(defschema VOID
  (eq nil))

(defmacro defrecord
  "Same as defrecord or schema.core/defrecord, except that the ->name and
   map->name constructors are made private. The strict-map->name constructor
   remains public if the schema version is used and the first argument after
   the name is the keyword :private."
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
         ~(when (and schema? (-> name meta :private))
            `(alter-meta! ~(get-var "strict-map->") assoc :private true)))))
