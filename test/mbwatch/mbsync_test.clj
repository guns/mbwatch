(ns mbwatch.mbsync-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [->log DEBUG ERR log-level]]
            [mbwatch.mbsync :as m]
            [mbwatch.process :refer [dump!]]
            [schema.test :as s])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(s/deftest test-join-mbargs
  (is (= "foo" (#'m/join-mbargs "foo" [])))
  (is (= "foo:bar" (#'m/join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (#'m/join-mbargs "foo" ["bar" "baz"]))))

(s/deftest test-spawn-sync
  (is (re-find #"No channels defined"
               (with-out-str (dump! (m/spawn-sync "" "" nil) :err *out*)))))

(s/deftest test-events
  (let [dt (DateTime.)
        start (m/strict-map->MbsyncEventStart
                {:level DEBUG
                 :mbchan "test"
                 :mboxes ["INBOX"]
                 :start dt})
        stop (m/strict-map->MbsyncEventStop
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
