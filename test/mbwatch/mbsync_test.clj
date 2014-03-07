(ns mbwatch.mbsync-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbsync :as m]
            [mbwatch.process :as process]
            [schema.test :as s]))

(s/deftest test-sync
  (is (re-find #"No channels defined"
               (with-out-str
                 (process/dump! (m/sync "" ["" nil]) :err *out*)))))
