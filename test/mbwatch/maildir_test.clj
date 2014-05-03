(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :refer [flatten-mbmap flatten-mbox
                                     get-all-mboxes get-mdir new-messages
                                     senders]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/maildir/foo-mdir/INBOX")

(deftest test-flatten-mbox
  (is (= (flatten-mbox "foo/bar/baz" ".")
         "foo.bar.baz"))
  (is (= (flatten-mbox "foo/bar/baz" nil)
         "foo/bar/baz")))

(deftest test-flatten-mbmap
  (is (= (flatten-mbmap {"foo" #{"bar/baz"}
                         "bar" #{"baz/foo"}
                         "baz" #{"foo/bar"}}
                        {"foo" {:inbox "inbox" :path "path" :flatten "."}
                         "bar" {:inbox "inbox" :path "path" :flatten "_"}
                         "baz" {:inbox "inbox" :path "path" :flatten nil}})
         {"foo" #{"bar.baz"}
          "bar" #{"baz_foo"}
          "baz" #{"foo/bar"}})))

(deftest test-get-mdir
  (let [maildir {:inbox "/home/user/Mail/INBOX"
                 :path "/home/user/Mail/gmail"
                 :flatten "."}]
    (is (= (get-mdir maildir "INBOX")
           "/home/user/Mail/INBOX"))
    (is (= (get-mdir maildir "clojure")
           "/home/user/Mail/gmail/clojure"))
    (is (= (get-mdir maildir "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail].Sent Mail"))
    (is (= (get-mdir (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail]/Sent Mail"))))

(deftest test-get-all-mboxes
  (is (= (get-all-mboxes {:path "test-resources/maildir/foo-mdir"
                          :inbox "test-resources/maildir/foo-mdir/INBOX"
                          :flatten nil})
         #{"INBOX" "clojure"})))

(deftest test-new-messages
  (is (= (count (new-messages TEST-MDIR 0))
         (count (filterv (fn [^File f]
                           (and (.isFile f) (re-find #"\A[^.]" (.getName f))))
                         (file-seq (File. TEST-MDIR)))))))

(deftest test-senders
  (is (= (senders (new-messages TEST-MDIR 0))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"
          "Alice <alice@example.com>"])))
