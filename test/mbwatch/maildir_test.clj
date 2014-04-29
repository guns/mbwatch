(ns mbwatch.maildir-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.maildir :refer [mdir-path new-messages senders]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/maildir/foo-mdir/INBOX")

(deftest test-new-messages
  (is (= (count (new-messages TEST-MDIR 0))
         (count (filterv #(.isFile ^File %) (file-seq (File. TEST-MDIR)))))))

(deftest test-senders
  (is (= (senders (new-messages TEST-MDIR 0))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"
          "Alice <alice@example.com>"])))

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
