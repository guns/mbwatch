(ns mbwatch.network-test
  (:require [clojure.test :refer [is]]
            [mbwatch.network :as n]
            [schema.test :refer [deftest]]))

(deftest test-lookup
  ;; Schema validation
  (n/lookup "example.com" 1000)
  (is (nil? (n/lookup "example.com" 0))))

(deftest test-reachable?
  ;; Schema validation
  (n/reachable? "example.com" 80 1000)
  (is (false? (n/reachable? "NXDOMAIN.example.com" 80 200))))
