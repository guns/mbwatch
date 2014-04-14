(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config :refer [->Config mdir-path]]
            [schema.test :refer [deftest]]))

(deftest test-Config
  ;; Schema validation
  (->Config (io/resource "mbsyncrc") (io/resource "mbwatchrc")))

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
