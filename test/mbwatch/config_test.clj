(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config :refer [->Config mdir-path]]
            [mbwatch.config.mbsyncrc]
            [schema.test :refer [deftest]])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(deftest test-Config
  (let [c (->Config (io/resource "mbsyncrc") (io/resource "config"))]
    (is (instance? Mbsyncrc (:mbsyncrc c)))
    (is (= (:notify-command c) "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= (:sync-timer-period c) (* 5 60 1000)))
    (is (= (:connection-period c) (* 15 60 1000)))
    (is (= (:connection-timeout c) 2000))
    (is (= (:imap-socket-timeout c) 5000))))

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
