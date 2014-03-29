(ns mbwatch.mbsync.events-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [->log DEBUG ERR WARNING log-level]]
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
    (is (= (->log start)
           (LogItem. DEBUG dt "Starting `mbsync test:INBOX`")))
    (is (= (->log stop)
           (LogItem. ERR dt "FAILURE: `mbsync test` aborted in zero seconds with status 1.\nERROR")))
    (is (= (->log unknown)
           (LogItem. WARNING dt "Unknown channel: `FOO`")))))
