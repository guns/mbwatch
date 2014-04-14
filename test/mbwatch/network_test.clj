(ns mbwatch.network-test
  (:require [clojure.test :refer [is]]
            [mbwatch.network :refer [reachable?]]
            [mbwatch.test.server :refer [poison-server with-server]]
            [schema.test :refer [deftest]]))

(deftest test-reachable?
  (with-server [host port]
    (is (reachable? host port 1000))
    (poison-server port)
    (is (false? (reachable? host port 1000)))))
