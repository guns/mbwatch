(ns mbwatch.util-test
  (:require [clojure.java.shell :refer [sh]]
            [clojure.test :refer [is]]
            [mbwatch.util :refer [catch-print chomp class-name dequote
                                  human-duration join-mbargs
                                  join-sync-request schema-params
                                  shell-escape to-ms url-for]]
            [schema.test :refer [deftest]])
  (:import (org.joda.time DateTime)))

(deftest test-join-mbargs
  (is (= "foo" (join-mbargs "foo" [])))
  (is (= "foo:bar" (join-mbargs "foo" ["bar"])))
  (is (= "foo:bar,baz" (join-mbargs "foo" ["bar" "baz"]))))

(deftest test-join-sync-request
  (is (= "bar:a baz:b,c foo"
         (join-sync-request {"foo" [] "bar" ["a"] "baz" ["c" "b"]}))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (schema-params '[foo :- Int bar baz :- #{Symbol}])
         (schema-params '[foo bar baz]))))

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

(deftest test-shell-escape
  (let [s (apply str (map char (range 0x01 0x80)))]
    (is (= (:out (sh "ruby" "-rshellwords" "-e" "print $stdin.read.shellescape" :in s))
           (shell-escape s)))))

(deftest test-human-duration
  (is (= "0 seconds" (human-duration 0)))
  (is (= "1 second" (human-duration 1000)))
  (is (= "59 seconds" (human-duration 59000)))
  (is (= "5 hours, 1 minute, and 35 seconds"
         (human-duration (+ (* 5 60 60 1000) (* 1 60 1000) (* 35 1000)))))
  (let [dt (DateTime.)]
    (is (= "1 second" (human-duration dt (.plus dt 1000))))))

(deftest test-class-name
  (is (= "String" (class-name ""))))

(deftest test-to-ms
  (is (= 0 (to-ms (DateTime. 0)))))

(deftest test-url-for
  (is (= "imaps://foo%40example.com@example.com:993"
         (url-for "imaps" "foo@example.com" "example.com" 993))))

(deftest test-catch-print
  (is (nil? (catch-print (throw (RuntimeException. "TESTING catch-print"))))))
