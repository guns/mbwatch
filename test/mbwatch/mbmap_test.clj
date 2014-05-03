(ns mbwatch.mbmap-test
  (:require [clojure.test :refer [is]]
            [mbwatch.mbmap :refer [join-mbentry join-mbmap mbmap->mbtuples
                                   mbmap-diff mbmap-diff+ mbmap-disj
                                   mbmap-intersection mbmap-merge
                                   mbmap-merge+ mbtuples->mbmap parse-mbargs
                                   parse-mbline]]
            [schema.test :refer [deftest]]))

(deftest test-parse-mbargs
  (is (= (parse-mbargs ["foo:bar,baz" "empty" "also-empty:" "quux:INBOX"])
         {"foo" #{"bar" "baz"}
          "empty" #{}
          "also-empty" #{}
          "quux" #{"INBOX"}})))

(deftest test-parse-mbline
  (is (= (parse-mbline "foo:'bar baz' bar:baz\\ foo")
         {"foo" #{"bar baz"}
          "bar" #{"baz foo"}})))

(deftest test-join-mbentry
  (is (= "foo" (join-mbentry "foo" #{})))
  (is (= "foo:bar" (join-mbentry "foo" #{"bar"})))
  (is (= "foo:bar,baz" (join-mbentry "foo" #{"bar" "baz"}))))

(deftest test-join-mbmap
  (is (= "bar:a baz:b,c foo"
         (join-mbmap (sorted-map "foo" #{} "bar" #{"a"} "baz" #{"c" "b"})))))

(deftest test-mbtuples
  (let [nmap {"α" #{"a" "b" "c"} "β" #{"a"} "γ" #{}}
        mbts (mbmap->mbtuples nmap)]
    (is (= mbts #{["α" "a"] ["α" "b"] ["α" "c"] ["β" "a"]}))
    (is (= (mbtuples->mbmap mbts) (dissoc nmap "γ")))))

(deftest test-mbmap-diff
  (is (= (mbmap-diff+ {"α" #{"b"} "β" #{"b"} "γ" #{"b"}}
                      {"α" #{"a" "c"} "β" #{"b" "c"}})
         [{"α" #{"b"} "γ" #{"b"}}
          {"α" #{"a" "c"} "β" #{"c"}}]))
  (is (= (mbmap-diff {"α" #{"b"} "β" #{} "γ" #{"b"} "Δ" #{} "ε" #{"b"}}
                     {"α" #{} "β" #{"b"} "γ" #{"a" "c"} "ε" #{"a" "b" "c"}})
         [{"γ" #{"b"} "Δ" #{}}
          {"α" #{} "γ" #{"a" "c"} "ε" #{"a" "c"}}])))

(deftest test-mbmap-intersection
  (is (= (mbmap-intersection {"α" #{"a" "b"} "β" #{"c" "d"}}
                             {"α" #{"a" "c"} "γ" #{"c" "d"}})
         {"α" #{"a"}})))

(deftest test-mbmap-disj
  (is (= (mbmap-disj {"α" #{"a" "b" "c"} "β" #{"a"} "γ" #{}}
                     {"α" #{"b" "c" "d"} "β" #{} "γ" #{"a"}})
         {"α" #{"a"} "γ" #{}})))

(deftest test-mbmap-merge
  (is (= (mbmap-merge+ {"α" #{"a"} "γ" #{"a"}}
                       {"β" #{"a"} "γ" #{"a" "b"} "Δ" #{"a"}})
         {"α" #{"a"} "β" #{"a"} "γ" #{"a" "b"} "Δ" #{"a"}}))
  (is (= (mbmap-merge {"α" #{"a"} "β" #{} "γ" #{"a"}}
                      {"α" #{} "β" #{"a"} "γ" #{"a" "b"} "Δ" #{"a"}})
         {"α" #{} "β" #{} "γ" #{"a" "b"} "Δ" #{"a"}})))
