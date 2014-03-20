(ns mbwatch.mbsync.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbsync.command :as c]
            [schema.test :as s])
  (:import (mbwatch.mbsync.command Command SyncCommand)))

(s/deftest test-command-ctor
  (is (= (c/->command :term)
         (Command. :term)))
  (is (= (c/->command nil)
         (Command. :stop)))
  (is (instance? SyncCommand (c/->command {"foo" []}))))
