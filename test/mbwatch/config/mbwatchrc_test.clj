(ns mbwatch.config.mbwatchrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.test :as s]))

(s/deftest test-parse
  (let [mbwatchrc (mbw/parse (slurp (io/resource "mbwatchrc")))]
    (is (= (:notify-cmd mbwatchrc) "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= (:imap-timeout mbwatchrc) 5000))))
