(ns mbwatch.core-test
  (:require [clojure.core.async :as a]
            [clojure.java.io :as io]
            [mbwatch.config :refer [->Config]]
            [mbwatch.core :as c]
            [schema.test :as s]))

(s/deftest test-Application
  ;; Schema validation
  (c/->Application
    (->Config (io/resource "mbsyncrc") (io/resource "mbwatchrc"))))
