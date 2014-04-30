(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [clojure.tools.cli :refer [get-default-options]]
            [mbwatch.config :refer [->Config config-options]]
            [mbwatch.config.mbsyncrc]
            [schema.core :refer [validate]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(deftest test-Config
  (let [{:keys [mbsyncrc idle sync notify blacklist notify-cmd conn-period
                sync-period conn-timeout imap-timeout]}
        (->Config {} (io/resource "mbsyncrc") (io/resource "mbwatchrc"))]
    (is (validate Mbsyncrc mbsyncrc))
    (is (= idle {"home" #{"INBOX"} "work" #{"INBOX"}}))
    (is (= sync {"home" #{} "work" #{} "school" #{}}))
    (is (= notify {"home" #{"INBOX"} "work" #{"INBOX" "clojure"} "school" #{"INBOX"}}))
    (is (= blacklist {"SPAM" #{}}))
    (is (= notify-cmd "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= conn-period 0))
    (is (= sync-period (* 10 60 1000)))
    (is (= conn-timeout 5000))
    (is (= imap-timeout 20000)))
  (is (= (-> (->Config {} (io/resource "mbsyncrc") "/does/not/exist")
             (dissoc :mbsyncrc))
         (get-default-options (config-options)))))
