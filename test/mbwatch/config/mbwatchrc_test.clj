(ns mbwatch.config.mbwatchrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.test :refer [deftest]]))

(deftest test-parse
  (let [mbwatchrc (mbw/parse (slurp (io/resource "mbwatchrc")))]
    (is (= (:notify-command mbwatchrc) "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= (:imap-socket-timeout mbwatchrc) 5000))))
