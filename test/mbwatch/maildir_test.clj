(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :as md]
            [schema.test :as s])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/maildir/foo-mdir/INBOX")

(s/deftest test-new-messages
  (is (= (count (md/new-messages TEST-MDIR 0))
         (count (filterv #(.isFile ^File %) (file-seq (File. TEST-MDIR)))))))

(s/deftest test-senders
  (is (= (md/senders (md/new-messages TEST-MDIR 0))
         #{"Alice <alice@example.com>"
           "★ ❤ Carol ❤ ★ <carol@example.com>"})))
