(ns mbwatch.events-test
  (:require [clojure.test :refer [is]]
            [mbwatch.events :refer [->MbsyncUnknownChannelError
                                    strict-map->MbsyncEventStart
                                    strict-map->MbsyncEventStop]]
            [mbwatch.logging :refer [DEBUG ERR WARNING log-item log-level]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(deftest test-events
  (let [dt (DateTime.)
        start (strict-map->MbsyncEventStart
                {:level DEBUG
                 :id 1
                 :mbchan "test"
                 :mboxes ["INBOX"]
                 :start dt})
        stop (strict-map->MbsyncEventStop
               {:level ERR
                :id 1
                :mbchan "test"
                :mboxes []
                :start dt
                :stop dt
                :status 1
                :error "ERROR"
                :maildir {:inbox "inbox/" :path "path/"}})
        unknown (->MbsyncUnknownChannelError 0 "FOO")]
    (is (= [DEBUG ERR WARNING]
           (map log-level [start stop unknown])))
    (is (= (log-item start)
           (LogItem. DEBUG dt "Starting `mbsync test:INBOX`")))
    (is (= (log-item stop)
           (LogItem. ERR dt "FAILURE: `mbsync test` aborted in 0 seconds with status 1.\nERROR")))
    (is (= (dissoc (log-item unknown) :timestamp)
           (dissoc (LogItem. WARNING dt "Unknown channel: `FOO`") :timestamp)))))