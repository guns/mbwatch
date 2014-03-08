(ns mbwatch.mbsync-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbsync :as m]
            [mbwatch.process :refer [dump!]]
            [schema.test :as s]))

(s/deftest test-spawn-sync
  (is (re-find #"No channels defined"
               (with-out-str (dump! (m/spawn-sync "" "" nil) :err *out*)))))
