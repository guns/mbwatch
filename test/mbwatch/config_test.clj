(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config :as c]
            [schema.test :as s]))

(s/deftest test-new-config
  ;; Schema validation
  (c/new-config (io/resource "mbsyncrc") (io/resource "mbwatchrc")))

(s/deftest test-mdir-path
  (let [maildir {:inbox "/home/user/Mail/INBOX"
                 :path "/home/user/Mail/gmail"
                 :flatten "."}]
    (is (= (c/mdir-path maildir "INBOX")
           "/home/user/Mail/INBOX"))
    (is (= (c/mdir-path maildir "clojure")
           "/home/user/Mail/gmail/clojure"))
    (is (= (c/mdir-path maildir "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail].Sent Mail"))
    (is (= (c/mdir-path (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail]/Sent Mail"))))
