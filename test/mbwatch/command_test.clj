(ns mbwatch.command-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.command :as c]))

;; A silly test to learn test.check
(defspec command-ids-are-unique 10
  (let [ctor-gen (g/tuple (g/elements (seq (:vs c/Opcode))) g/any)]
    (for-all [ctors (g/vector ctor-gen 2 100)]
      (apply distinct? (doall (pmap (fn [[o p]] (:id (c/->Command o p))) ctors))))))
