(ns mbwatch.config.mbwatchrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.test :refer [deftest]]))

(deftest test-parse
  (let [mbwatchrc (mbw/parse (slurp (io/resource "mbwatchrc")))]
    (is (= (:notify-command mbwatchrc) "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= (:sync-timer-period mbwatchrc) (* 5 60 1000)))
    (is (= (:connection-period mbwatchrc) (* 15 60 1000)))
    (is (= (:connection-timeout mbwatchrc) 2000))
    (is (= (:imap-socket-timeout mbwatchrc) 5000))))
