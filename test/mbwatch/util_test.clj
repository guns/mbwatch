(ns mbwatch.util-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.util :refer [catch-print chomp class-name dequote dt->ms
                                  human-duration istr= join-mbentry
                                  join-mbmap mbmap->mbtuples mbmap-diff
                                  mbmap-disj mbtuples->mbmap parse-kv-string
                                  parse-mbargs parse-ms schema-params url-for
                                  zero-or-min]]
            [schema.test :refer [deftest]])
  (:import (org.joda.time DateTime)))

(deftest test-parse-args
  (is (= (parse-mbargs ["foo:bar,baz" "empty" "also-empty:" "quux:INBOX"])
         {"foo" #{"bar" "baz"}
          "empty" #{"INBOX"}
          "also-empty" #{"INBOX"}
          "quux" #{"INBOX"}})))

(deftest test-join-mbentry
  (is (= "foo" (join-mbentry "foo" #{})))
  (is (= "foo:bar" (join-mbentry "foo" #{"bar"})))
  (is (= "foo:bar,baz" (join-mbentry "foo" #{"bar" "baz"}))))

(deftest test-join-mbmap
  (is (= "bar:a baz:b,c foo"
         (join-mbmap (sorted-map "foo" #{} "bar" #{"a"} "baz" #{"c" "b"})))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (schema-params '[foo :- Int bar baz :- #{Symbol}])
         (schema-params '[foo bar baz]))))

(deftest test-parse-kv-string
  (is (= (parse-kv-string (slurp (io/resource "test-parse-kv-string.in")))
         {(keyword "α-α-α") "alpha, \"the first letter\""
          (keyword "β β β") "beta, 'the second letter'"
          (keyword "γ_γ\tγ") "gamma, #the ;third # letter"}))
  (is (thrown? RuntimeException (parse-kv-string "foo bar"))))

(deftest test-istr=
  (is (istr= "foo" "foo"))
  (is (istr= "ΩMEGA" "ωmega"))
  (is (not (istr= "Alpha" "αlfa"))))

(deftest test-chomp
  (is (= ""    (chomp "") (chomp "" "")))
  (is (= "foo" (chomp "foo\r\n")))
  (is (= "foo" (chomp "foo" "")))
  (is (= "foo" (chomp "foooo" "oo")))
  (is (= "foo" (chomp "foo.bar" ".bar")))
  (is (= "foo" (chomp "foo/" "/")))
  (is (= "foo" (chomp "foo" "f"))))

(deftest test-dequote
  (is (empty? (dequote "\"\"")))
  (is (= "ab\\c\"" (dequote "\"ab\\\\c\\\"\"")))
  (is (= "ab\\c" (dequote "ab\\c"))))

(deftest test-human-duration
  (is (= "0 seconds" (human-duration 0)))
  (is (= "1 second" (human-duration 1000)))
  (is (= "59 seconds" (human-duration 59000)))
  (is (= "5 hours, 1 minute, and 35 seconds"
         (human-duration (+ (* 5 60 60 1000) (* 1 60 1000) (* 35 1000)))))
  (let [dt (DateTime.)]
    (is (= "1 second" (human-duration dt (.plus dt 1000))))))

(deftest test-parse-ms
  (is (= 0 (parse-ms "")))
  (is (= (Math/round (+ (* 1.5 24 60 60 1000)
                        (* 1.0 60 60 1000)
                        (* 1.5 60 1000)
                        1000
                        1))
         (parse-ms "1.5d1h 1.5m1s\t1ms"))))

(deftest test-dt->ms
  (is (= 0 (dt->ms (DateTime. 0)))))

(deftest test-class-name
  (is (= "String" (class-name ""))))

(deftest test-zero-or-min
  (is (every? zero? (mapv #(zero-or-min % 0) [-1 0 Long/MIN_VALUE])))
  (is (every? #(= 5000 %) (mapv #(zero-or-min % 5000) [1 4999 5000]))))

(deftest test-url-for
  (is (= "imaps://foo%40example.com@example.com:993"
         (url-for "imaps" "foo@example.com" "example.com" 993))))

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

(deftest test-catch-print
  (is (nil? (catch-print (throw (RuntimeException. "TESTING catch-print"))))))
