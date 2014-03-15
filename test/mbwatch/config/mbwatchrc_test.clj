(ns mbwatch.config.mbwatchrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.test :as s]))

(s/deftest test-parse
  (let [mbwatchrc (mbw/parse (slurp (io/resource "mbwatchrc")))]
    (is (= (:notification-cmd mbwatchrc)
           "notify - -a \"~/.sounds/new-message.mp3\""))))
