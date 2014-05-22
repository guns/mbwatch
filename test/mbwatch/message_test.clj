(ns mbwatch.message-test
  (:require [clojure.test :refer [is]]
            [mbwatch.message :refer [headers message senders subject]]
            [schema.test :refer [deftest]]))

(deftest test-all
  (is (= (senders (message "test-resources/Maildirs/foo-mdir/INBOX/new/1394903570.11025_1.MBWATCH"))
         ["Alice <alice@example.com>"]))
  (is (= (senders (message "test-resources/Maildirs/foo-mdir/INBOX/new/1394904343.11337_1.MBWATCH"))
         ["★ ❤ Carol ❤ ★ <carol@example.com>"]))
  (is (= (subject (message "test-resources/Maildirs/foo-mdir/INBOX/new/1394904343.11337_1.MBWATCH"))
         "My public key"))
  (is (= (headers (message "test-resources/Maildirs/foo-mdir/INBOX/new/1394904343.11337_1.MBWATCH")
                  "content-type")
         ["text/plain; charset=us-ascii"])))
