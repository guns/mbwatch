(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :refer [new-messages senders]]
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
