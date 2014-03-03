(ns mbwatch.util
  (:require [schema.core :as s])
  (:refer-clojure :exclude [defn-]))

(defmacro defn-
  "Private version of schema.core/defn"
  {:require [#'s/defn]}
  [name & body]
  `(s/defn ~(with-meta name (assoc (meta name) :private true))
     ~@body))
