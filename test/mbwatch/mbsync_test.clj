(ns mbwatch.mbsync-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbsync :as m]
            [mbwatch.process :refer [dump!]]
            [schema.test :as s]))

(s/deftest test-join-mbargs
  (is (= "foo" (#'m/join-mbargs "foo" [])))
  (is (= "foo:bar" (#'m/join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (#'m/join-mbargs "foo" ["bar" "baz"]))))

(s/deftest test-spawn-sync
  (is (re-find #"No channels defined"
               (with-out-str (dump! (m/spawn-sync "" "" nil) :err *out*)))))
