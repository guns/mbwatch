(ns mbwatch.network-test
  (:require [clojure.test :refer [is]]
            [mbwatch.network :as n]
            [mbwatch.test.server :refer [poison-server with-server]]
            [schema.test :refer [deftest]]))

(deftest test-reachable?
  (with-server [host port]
    (is (n/reachable? host port 1000))
    (poison-server port)
    (is (false? (n/reachable? host port 1000)))))
