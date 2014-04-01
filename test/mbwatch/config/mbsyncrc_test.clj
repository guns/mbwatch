(ns mbwatch.config.mbsyncrc-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.config.mbsyncrc :as mb]
            [schema.test :as s]))

(s/deftest test-parse
  (let [mbsyncrc (mb/parse (slurp (io/resource "mbsyncrc")))]
    (is (= (:text mbsyncrc)
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
                        "useimaps" "yes"
                        "requiressl" "yes"
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
    (is (= (:channels mbsyncrc)
           #{"FOO-chan" "BAR-chan" "FOO-BAR-chan" "FOO-ROOT-chan"}))
    #_(is (= (:imapstore-names->credentials mbsyncrc)
           {"FOO-imap" {:host "imap.example.com"
                        :user "foo@example.com"
                        :port 993
                        :pass "@Y9GZa G!Dsl ZQ'PC(Gj5#6`-Sv->$xH0s{5|bMgq/0.R&g.u714\"; F3aN"}
            "BAR-imap" {:host "imap.example.com"
                        :user "bar@example.com"
                        :port 993
                        :pass "H'|&z]0pIcU2?T/(<!zaIq[wW\\PnDvb%%I,_n7*)'yJLqoTfcu>bYn1:xYc\""}}))
    (is (= (:channel->Maildirstore mbsyncrc)
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
