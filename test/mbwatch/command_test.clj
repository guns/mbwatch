(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :as c]
            [schema.test :as s]))

(s/deftest test-Command
  ;; Schema validation
  (c/->Command :term)
  (c/->Command :sync {"foo" ["bar"]})
  (is (thrown? Exception (c/->Command :UNKNOWN))))
