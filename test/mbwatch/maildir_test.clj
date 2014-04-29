(ns mbwatch.maildir-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.maildir :refer [get-all-mdirs get-mdir new-messages
                                     senders]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/maildir/foo-mdir/INBOX")

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

(deftest test-get-all-mdirs
  (is (= (into #{} (mapv #(str "test-resources/maildir/foo-mdir/" %) ["INBOX" "clojure"]))
         (get-all-mdirs {:path "test-resources/maildir/foo-mdir"
                         :inbox "test-resources/maildir/foo-mdir/INBOX"
                         :flatten nil}))))

(deftest test-new-messages
  (is (= (count (new-messages TEST-MDIR 0))
         (count (filterv (fn [^File f]
                           (and (.isFile f) (re-find #"\A[^.]" (.getName f))))
                         (file-seq (File. TEST-MDIR)))))))

(deftest test-senders
  (is (= (senders (new-messages TEST-MDIR 0))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"
          "Alice <alice@example.com>"])))
