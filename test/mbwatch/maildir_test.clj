(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :refer [flatten-mbmap flatten-mbox
                                     get-all-mboxes get-mdir new-messages
                                     senders]]
            [mbwatch.types :refer [strict-map->Maildirstore]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/Maildirs/foo-mdir/INBOX")

(deftest test-flatten-mbox
  (is (= (flatten-mbox "foo/bar/baz" ".")
         "foo.bar.baz"))
  (is (= (flatten-mbox "foo/bar/baz" nil)
         "foo/bar/baz")))

(deftest test-flatten-mbmap
  (is (= (flatten-mbmap {"foo" #{"bar/baz"}
                         "bar" #{"baz/foo"}
                         "baz" #{"foo/bar"}}
                        {"foo" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten "."})
                         "bar" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten "_"})
                         "baz" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten nil})})
         {"foo" #{"bar.baz"}
          "bar" #{"baz_foo"}
          "baz" #{"foo/bar"}})))

(deftest test-get-mdir
  (let [maildir (strict-map->Maildirstore
                  {:inbox "/home/user/Mail/INBOX"
                   :path "/home/user/Mail/gmail"
                   :flatten "."})]
    (is (= (get-mdir maildir "INBOX")
           "/home/user/Mail/INBOX"))
    (is (= (get-mdir maildir "clojure")
           "/home/user/Mail/gmail/clojure"))
    (is (= (get-mdir maildir "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail].Sent Mail"))
    (is (= (get-mdir (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail]/Sent Mail"))))

(deftest test-get-all-mboxes
  (is (= (get-all-mboxes (strict-map->Maildirstore
                           {:path "test-resources/Maildirs/foo-mdir"
                            :inbox "test-resources/Maildirs/foo-mdir/INBOX"
                            :flatten nil}))
         #{"INBOX" "test"})))

(deftest test-new-messages
  (is (= (count (new-messages TEST-MDIR 0))
         (count (filterv (fn [^File f]
                           (and (.isFile f) (not (.isHidden f))))
                         (file-seq (File. TEST-MDIR)))))))

(deftest test-senders
  (is (= (senders (new-messages TEST-MDIR 0))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"
          "Alice <alice@example.com>"])))
