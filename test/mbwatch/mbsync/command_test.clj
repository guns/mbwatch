(ns mbwatch.mbsync.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbsync.command :as c]
            [schema.test :as s])
  (:import (mbwatch.mbsync.command Command SyncCommand)))

(s/deftest test-command-ctor
  (is (instance? Command (c/->command :term)))
  (is (instance? SyncCommand (c/->command {})))
  (is (= :stop (c/command (c/->command nil)))))
