(ns mbwatch.mbsync.events-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [DEBUG ERR WARNING log-item log-level]]
            [mbwatch.mbsync.events :as e]
            [schema.test :refer [deftest]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(deftest test-join-mbargs
  (is (= "foo" (e/join-mbargs "foo" [])))
  (is (= "foo:bar" (e/join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (e/join-mbargs "foo" ["bar" "baz"]))))

(deftest test-events
  (let [dt (DateTime.)
        start (e/strict-map->MbsyncEventStart
                {:level DEBUG
                 :id 1
                 :mbchan "test"
                 :mboxes ["INBOX"]
                 :start dt})
        stop (e/strict-map->MbsyncEventStop
               {:level ERR
                :id 1
                :mbchan "test"
                :mboxes []
                :start dt
                :stop dt
                :status 1
                :error "ERROR"
                :maildir {:inbox "inbox/" :path "path/"}})
        unknown (e/strict-map->MbsyncUnknownChannelError
                  {:id 0
                   :mbchan "FOO"
                   :timestamp dt})]
    (is (= [DEBUG ERR WARNING]
           (map log-level [start stop unknown])))
    (is (= (log-item start)
           (LogItem. DEBUG dt "Starting `mbsync test:INBOX`")))
    (is (= (log-item stop)
           (LogItem. ERR dt "FAILURE: `mbsync test` aborted in zero seconds with status 1.\nERROR")))
    (is (= (log-item unknown)
           (LogItem. WARNING dt "Unknown channel: `FOO`")))))
