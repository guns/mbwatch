(ns mbwatch.console-logger-test
  (:require [clojure.test :refer [deftest is]]
            [mbwatch.console-logger :as c]
            [mbwatch.logging :refer [->log DEBUG ERR]]
            [mbwatch.mbsync :as mb]
            [schema.core :refer [validate]]
            [schema.test :as s])
  (:import (clojure.lang Keyword)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(deftest test-sgr
  (is (validate {Keyword String} c/sgr)))

(s/deftest test-Loggable
  (let [dt (DateTime.)]
    (is (= (->log (mb/strict-map->MbsyncEventStart
                    {:level DEBUG
                     :mbchan "test"
                     :mboxes []
                     :start dt}))
           (LogItem. DEBUG dt "Starting `mbsync test`")))
    (is (= (->log (mb/strict-map->MbsyncEventStop
                    {:level ERR
                     :mbchan "test"
                     :mboxes []
                     :start dt
                     :stop dt
                     :status 1
                     :error "ERROR"}))
           (LogItem. ERR dt "FAILURE: `mbsync test` aborted in zero seconds with status 1.\nERROR")))))
