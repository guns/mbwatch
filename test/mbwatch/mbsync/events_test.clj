(ns mbwatch.mbsync.events-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [->log DEBUG ERR log-level]]
            [mbwatch.mbsync.events :as e]
            [schema.test :as s])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(s/deftest test-join-mbargs
  (is (= "foo" (e/join-mbargs "foo" [])))
  (is (= "foo:bar" (e/join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (e/join-mbargs "foo" ["bar" "baz"]))))

(s/deftest test-events
  (let [dt (DateTime.)
        start (e/strict-map->MbsyncEventStart
                {:level DEBUG
                 :mbchan "test"
                 :mboxes ["INBOX"]
                 :start dt})
        stop (e/strict-map->MbsyncEventStop
               {:level ERR
                :mbchan "test"
                :mboxes []
                :start dt
                :stop dt
                :status 1
                :error "ERROR"
                :maildir {:inbox "inbox/" :path "path/"}})]
    (is (= DEBUG (log-level start)))
    (is (= ERR (log-level stop)))
    (is (= (->log start)
           (LogItem. DEBUG dt "Starting `mbsync test:INBOX`")))
    (is (= (->log stop)
           (LogItem. ERR dt "FAILURE: `mbsync test` aborted in zero seconds with status 1.\nERROR")))))
