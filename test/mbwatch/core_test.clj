(ns mbwatch.core-test
  (:require [clojure.core.async :as a]
            [clojure.java.io :as io]
            [mbwatch.config :refer [new-config]]
            [mbwatch.core :as c]
            [schema.test :as s]))

;; TODO: Integration tests

(s/deftest test-new-application
  ;; Schema validation
  (c/new-application
    (new-config (io/resource "mbsyncrc") (io/resource "mbwatchrc"))
    (a/chan)))
