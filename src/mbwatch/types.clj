(ns mbwatch.types
  (:require [clojure.core.async.impl.protocols :refer [Buffer]]
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
