(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :as md]
            [schema.test :as s])
  (:import (java.io File)))

(def ^String test-mdir
  "test-resources/maildir/foo-mdir/INBOX")

(s/deftest test-new-messages
  (is (= (count (md/new-messages test-mdir 0))
         (count (filter #(.isFile ^File %) (file-seq (File. test-mdir)))))))

(s/deftest test-senders
  (is (= (md/senders (md/new-messages test-mdir 0))
         #{"Alice <alice@example.com>"
           "★ ❤ Carol ❤ ★ <carol@example.com>"})))
