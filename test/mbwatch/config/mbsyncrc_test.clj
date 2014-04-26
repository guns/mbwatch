(ns mbwatch.config.mbsyncrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbsyncrc :refer [get-password parse-mbsyncrc]]
            [mbwatch.util :refer [chomp]]
            [schema.test :refer [deftest]]))

(deftest test-get-password
  (is (= (get-password "cat test-resources/foo@example.com.pass")
         (chomp (slurp (io/resource "foo@example.com.pass")))))
  (is (= (get-password (.getBytes "correct horse battery staple"))
         "correct horse battery staple"))
  (is (nil? (get-password "echo >&2 testing get-password; false"))))

(deftest test-parse
  (let [mbsyncrc (parse-mbsyncrc (slurp (io/resource "mbsyncrc")))]
    (is (= ((:render-fn mbsyncrc))
           (slurp (io/resource "mbsyncrc.out"))))
    (is (= (-> mbsyncrc :sections :general)
           ["Create Both"
            "Expunge Both"]))
    (is (= (-> mbsyncrc :sections :imapstore)
           {"FOO-imap" {"host" "imap.example.com"
                        "user" "foo@example.com"
                        "useimaps" "yes"
                        "requiressl" "yes"
                        "passcmd" "\"cat test-resources/foo@example.com.pass\""}
            "BAR-imap" {"host" "imap.example.com"
                        "user" "bar@example.com"
                        "port" "993"
                        "useimaps" "no"
                        "requiressl" "no"
                        "pass" "\"H'|&z]0pIcU2?T/(<!zaIq[wW\\\\PnDvb%%I,_n7*)'yJLqoTfcu>bYn1:xYc\\\"\""}}))
    (is (= (-> mbsyncrc :sections :maildirstore)
           {"FOO-mdir" {"inbox" "test-resources/maildir/foo-mdir/INBOX"
                        "path" "test-resources/maildir/foo-mdir/"
                        "flatten" "."}
            "BAR-mdir" {"path" "test-resources/maildir/bar-mdir/"}
            "ROOT-mdir" {"inbox" "~root/Mail/INBOX"
                         "path" "~root/Mail/root/"}}))
    (is (= (-> mbsyncrc :sections :channel)
           {"FOO-chan" {"master" ":FOO-imap:"
                        "slave" ":FOO-mdir:"
                        "patterns" "*"}
            "BAR-chan" {"master" ":BAR-imap:"
                        "slave" ":BAR-mdir:"}
            "FOO-BAR-chan" {"master" ":FOO-mdir:"
                            "slave" ":BAR-mdir:"
                            "patterns" "*"
                            "sync" "All"
                            "create" "Both"
                            "expunge" "Both"}
            "FOO-ROOT-chan"{"master" ":FOO-mdir:"
                            "slave" ":ROOT-mdir:"
                            "patterns" "*"
                            "sync" "All"
                            "create" "Both"
                            "expunge" "Both"}}))
    (is (= (-> mbsyncrc :mbchans)
           #{"FOO-chan" "BAR-chan" "FOO-BAR-chan" "FOO-ROOT-chan"}))
    (is (= (update-in (-> mbsyncrc :mbchan->IMAPCredential) ["BAR-chan" :pass] seq)
           {"FOO-chan" {:host "imap.example.com"
                        :user "foo@example.com"
                        :port 993
                        :pass "cat test-resources/foo@example.com.pass"
                        :ssl? true}
            "BAR-chan" {:host "imap.example.com"
                        :user "bar@example.com"
                        :port 993
                        :pass (seq (.getBytes "H'|&z]0pIcU2?T/(<!zaIq[wW\\PnDvb%%I,_n7*)'yJLqoTfcu>bYn1:xYc\""))
                        :ssl? false}}))
    (is (= (-> mbsyncrc :mbchan->Maildirstore)
           {"FOO-chan" {:inbox "test-resources/maildir/foo-mdir/INBOX"
                        :path "test-resources/maildir/foo-mdir/"
                        :flatten "."}
            "BAR-chan" {:inbox "/home/guns/Maildir"
                        :path "test-resources/maildir/bar-mdir/"
                        :flatten nil}
            "FOO-BAR-chan" {:inbox "/home/guns/Maildir"
                            :path "test-resources/maildir/bar-mdir/"
                            :flatten nil}
            "FOO-ROOT-chan" {:inbox "/root/Mail/INBOX"
                             :path "/root/Mail/root/"
                             :flatten nil}}))))
