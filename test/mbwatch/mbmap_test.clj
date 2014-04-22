(ns mbwatch.mbmap-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbmap :refer [join-mbentry join-mbmap mbmap->mbtuples
                                   mbmap-diff mbmap-disj mbtuples->mbmap
                                   parse-mbargs]]
            [schema.test :refer [deftest]]))

(deftest test-parse-mbargs
  (is (= (parse-mbargs ["foo:bar,baz" "empty" "also-empty:" "quux:INBOX"])
         {"foo" #{"bar" "baz"}
          "empty" #{}
          "also-empty" #{}
          "quux" #{"INBOX"}})))

(deftest test-join-mbentry
  (is (= "foo" (join-mbentry "foo" #{})))
  (is (= "foo:bar" (join-mbentry "foo" #{"bar"})))
  (is (= "foo:bar,baz" (join-mbentry "foo" #{"bar" "baz"}))))

(deftest test-join-mbmap
  (is (= "bar:a baz:b,c foo"
         (join-mbmap (sorted-map "foo" #{} "bar" #{"a"} "baz" #{"c" "b"})))))

(deftest test-mbtuples
  (let [nmap {"α" #{"a" "b" "c"} "β" #{"a"}}
        mbts (mbmap->mbtuples nmap)]
    (is (= mbts #{["α" "a"] ["α" "b"] ["α" "c"] ["β" "a"]}))
    (is (= (mbtuples->mbmap mbts) nmap))))

(deftest test-mbmap-diff
  (is (= (mbmap-diff {"α" #{"b"} "β" #{"b"} "γ" #{"b"}}
                     {"α" #{"a" "c"} "β" #{"b" "c"}})
         [#{["α" "b"] ["γ" "b"]}
          #{["α" "a"] ["α" "c"] ["β" "c"]}])))

(deftest test-mbmap-disj
  (is (= (mbmap-disj {"α" #{"a" "b" "c"} "β" #{"a"}}
                     {"α" #{"b" "c" "d"} "β" #{"a"}})
         {"α" #{"a"}})))
