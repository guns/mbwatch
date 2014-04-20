(ns mbwatch.core-test
  (:require [clojure.java.io :as io]
            [mbwatch.config :refer [->Config]]
            [mbwatch.core :refer [->Application]]
            [schema.test :as s]))

(s/deftest test-Application
  ;; Schema validation
  (->Application
    (->Config (io/resource "mbsyncrc") (io/resource "config"))))
