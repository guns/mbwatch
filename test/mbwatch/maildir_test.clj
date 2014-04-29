(ns mbwatch.maildir-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.maildir :refer [get-mdir new-messages senders]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/maildir/foo-mdir/INBOX")

(deftest test-get-mdir
  (let [maildir {:inbox "/home/user/Mail/INBOX"
                 :path "/home/user/Mail/gmail"
                 :flatten "."}]
    (is (= (get-mdir maildir "INBOX")
           (io/file "/home/user/Mail/INBOX")))
    (is (= (get-mdir maildir "clojure")
           (io/file "/home/user/Mail/gmail/clojure")))
    (is (= (get-mdir maildir "[Gmail]/Sent Mail")
           (io/file "/home/user/Mail/gmail/[Gmail].Sent Mail")))
    (is (= (get-mdir (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           (io/file "/home/user/Mail/gmail/[Gmail]/Sent Mail")))))

(deftest test-new-messages
  (is (= (count (new-messages TEST-MDIR 0))
         (count (filterv #(.isFile ^File %) (file-seq (File. TEST-MDIR)))))))

(deftest test-senders
  (is (= (senders (new-messages TEST-MDIR 0))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"
          "Alice <alice@example.com>"])))
