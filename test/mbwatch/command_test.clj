(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :as c]
            [schema.test :refer [deftest]]))

(deftest test-Command
  ;; Schema validation
  (c/->Command :sync/term)
  (c/->Command :sync {"foo" ["bar"]})
  (is (thrown? Exception (c/->Command :UNKNOWN))))
