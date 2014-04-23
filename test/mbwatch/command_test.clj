(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :refer [->Command parse-command-input]]
            [schema.test :refer [deftest]]))

(deftest test-command-ids-are-unique
  (is (distinct? (->> (range 1000)
                      (pmap (fn [_] (:id (->Command :sync/term nil))))
                      doall))))

(deftest test-parse-command-input
  (parse-command-input "s"))
