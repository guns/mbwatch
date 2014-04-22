(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :refer [->Command]]
            [schema.test :refer [deftest]]))

(deftest test-command-ids-are-unique
  (is (distinct? (->> (range 1000)
                      (pmap (fn [_] (:id (->Command :sync/kill nil))))
                      doall))))
