(ns mbwatch.util-test
  (:require [clojure.java.shell :refer [sh]]
            [clojure.test :refer [is]]
            [mbwatch.util :as u]
            [schema.test :refer [deftest]])
  (:import (org.joda.time DateTime)))

(deftest test-join-mbargs
  (is (= "foo" (u/join-mbargs "foo" [])))
  (is (= "foo:bar" (u/join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (u/join-mbargs "foo" ["bar" "baz"]))))

(deftest test-join-sync-request
  (is (= "bar:a baz:b,c foo"
         (u/join-sync-request {"foo" [] "bar" ["a"] "baz" ["c" "b"]}))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (u/schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (u/schema-params '[foo :- Int bar baz :- #{Symbol}])
         (u/schema-params '[foo bar baz]))))

(deftest test-chomp
  (is (= ""    (u/chomp "") (u/chomp "" "")))
  (is (= "foo" (u/chomp "foo\r\n")))
  (is (= "foo" (u/chomp "foo" "")))
  (is (= "foo" (u/chomp "foooo" "oo")))
  (is (= "foo" (u/chomp "foo.bar" ".bar")))
  (is (= "foo" (u/chomp "foo/" "/")))
  (is (= "foo" (u/chomp "foo" "f"))))

(deftest test-dequote
  (is (empty? (u/dequote "\"\"")))
  (is (= "ab\\c\"" (u/dequote "\"ab\\\\c\\\"\"")))
  (is (= "ab\\c" (u/dequote "ab\\c"))))

(deftest test-shell-escape
  (let [s (apply str (map char (range 0x01 0x80)))]
    (is (= (:out (sh "ruby" "-rshellwords" "-e" "print $stdin.read.shellescape" :in s))
           (u/shell-escape s)))))

(deftest test-human-duration
  (is (= "0 seconds" (u/human-duration 0)))
  (is (= "1 second" (u/human-duration 1000)))
  (is (= "59 seconds" (u/human-duration 59000)))
  (is (= "5 hours, 1 minute, and 35 seconds"
         (u/human-duration (+ (* 5 60 60 1000) (* 1 60 1000) (* 35 1000)))))
  (let [dt (DateTime.)]
    (is (= "1 second" (u/human-duration dt (.plus dt 1000))))))

(deftest test-class-name
  (is (= "String" (u/class-name ""))))

(deftest test-to-ms
  (is (= 0 (u/to-ms (DateTime. 0)))))

(deftest test-url-for
  (is (= "imaps://foo%40example.com@example.com:993"
         (u/url-for "imaps" "foo@example.com" "example.com" 993))))

(deftest test-catch-print
  (is (nil? (u/catch-print (assert false)))))
