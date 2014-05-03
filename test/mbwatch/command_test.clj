(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :refer [->Command parse-command-input]]
            [mbwatch.concurrent :refer [pmapv]]
            [schema.test :refer [deftest]]))

(deftest test-command-ids-are-unique
  (is (distinct? (->> (vec (range 1000))
                      (pmapv (fn [_] (:id (->Command :sync/term nil))))
                      doall))))

(deftest test-command-input-parsing
  (is (re-find #"(?i)ambiguous" (parse-command-input "i")))
  (is (re-find #"(?i)unrecognized" (parse-command-input "ZZZZ")))
  (is (re-find #"(?i)requires" (parse-command-input "idle add")))
  (is (= (:payload (parse-command-input "s p 10m 30s"))
         (+ (* 10 60 1000) (* 30 1000)))))
