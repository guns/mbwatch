(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config :refer [->Config mdir-path]]
            [mbwatch.config.mbsyncrc]
            [schema.core :refer [validate]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(deftest test-Config
  (let [{:keys [mbsyncrc idle sync notify notify-cmd conn-period sync-period
                conn-timeout imap-timeout]}
        (->Config {} (io/resource "mbsyncrc") (io/resource "mbwatchrc"))]
    (is (validate Mbsyncrc mbsyncrc))
    (is (= idle {"home" #{"INBOX"} "work" #{"INBOX"}}))
    (is (= sync {"home" #{} "work" #{}}))
    (is (= notify {"home" #{"INBOX"} "work" #{"INBOX" "clojure"} "school" #{"INBOX"}}))
    (is (= notify-cmd "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= conn-period 0))
    (is (= sync-period (* 10 60 1000)))
    (is (= conn-timeout 5000))
    (is (= imap-timeout 20000))))

(deftest test-mdir-path
  (let [maildir {:inbox "/home/user/Mail/INBOX"
                 :path "/home/user/Mail/gmail"
                 :flatten "."}]
    (is (= (mdir-path maildir "INBOX")
           "/home/user/Mail/INBOX"))
    (is (= (mdir-path maildir "clojure")
           "/home/user/Mail/gmail/clojure"))
    (is (= (mdir-path maildir "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail].Sent Mail"))
    (is (= (mdir-path (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail]/Sent Mail"))))
