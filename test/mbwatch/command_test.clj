(ns mbwatch.command-test
  (:require [clojure.test :refer [is]]
            [mbwatch.command :as c]
            [mbwatch.types :refer [command]]
            [schema.test :as s])
  (:import (mbwatch.command Command SyncCommand)))

(s/deftest test-command-ctor
  (is (instance? Command (c/->ICommand :term)))
  (is (instance? SyncCommand (c/->ICommand {})))
  (is (= :stop (command (c/->ICommand nil)))))
