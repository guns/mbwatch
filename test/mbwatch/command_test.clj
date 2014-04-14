(ns mbwatch.command-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [elements tuple]]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.command :refer [->Command Opcode]]))

;; A silly test to learn test.check
(defspec command-ids-are-unique 10
  (let [ctor-gen (tuple (elements (seq (:vs Opcode))) g/any)]
    (for-all [ctors (g/vector ctor-gen 2 100)]
      (apply distinct? (doall (pmap (fn [[o p]] (:id (->Command o p))) ctors))))))
