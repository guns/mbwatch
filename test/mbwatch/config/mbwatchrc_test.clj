(ns mbwatch.config.mbwatch-test
  (:require [clojure.java.io :as io]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.test :as s]))

(s/deftest test-parse
  (mbw/parse (slurp (io/resource "test-resources/mbwatchrc"))))
