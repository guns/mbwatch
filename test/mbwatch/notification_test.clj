(ns mbwatch.notification-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [DEBUG]]
            [mbwatch.mbsync.events :as e]
            [mbwatch.notification :as n]
            [schema.test :as s])
  (:import (org.joda.time DateTime)))

(s/deftest test-new-message-notification
  (is (= "[FOO-chan/INBOX]\t2 new messages from:\nAlice <alice@example.com>\n★ ❤ Carol ❤ ★ <carol@example.com>"
         (#'n/new-message-notification
           {"FOO-chan" #{"INBOX"}}
           [(e/strict-map->MbsyncEventStop
              {:level DEBUG
               :id 1
               :mbchan "FOO-chan"
               :mboxes []
               :start (DateTime. 0)
               :stop (DateTime.)
               :status 0
               :error nil
               :maildir {:inbox "test-resources/maildir/foo-mdir/INBOX"
                         :path "test-resources/maildir/foo-mdir/"
                         :flatten nil}})
            (e/strict-map->MbsyncEventStop
              {:level DEBUG
               :id 1
               :mbchan "BAR-chan"
               :mboxes []
               :start (DateTime. 0)
               :stop (DateTime.)
               :status 0
               :error nil
               :maildir {:inbox "test-resources/maildir/bar-mdir/INBOX"
                         :path "test-resources/maildir/bar-mdir/"
                         :flatten nil}})])))
  (is (re-find #"\A\[FOO-chan/clojure\]\t17 new messages from:"
               (#'n/new-message-notification
                 {"FOO-chan" #{"clojure"}}
                 [(e/strict-map->MbsyncEventStop
                    {:level DEBUG
                     :id 2
                     :mbchan "FOO-chan"
                     :mboxes []
                     :start (DateTime. 0)
                     :stop (DateTime.)
                     :status 0
                     :error nil
                     :maildir {:inbox "test-resources/maildir/foo-mdir/INBOX"
                               :path "test-resources/maildir/foo-mdir/"
                               :flatten nil}})]))))
