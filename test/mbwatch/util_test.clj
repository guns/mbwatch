(ns mbwatch.util-test
  (:require [clojure.java.shell :refer [sh]]
            [clojure.test :refer [deftest is]]
            [mbwatch.util :as u])
  (:import (org.joda.time DateTime)))

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
  (is (= "zero seconds" (u/human-duration 0)))
  (is (= "1 second" (u/human-duration 1)))
  (is (= "59 seconds" (u/human-duration 59)))
  (is (= "5 hours, 1 minute, and 35 seconds"
         (u/human-duration (+ (* 5 60 60) (* 1 60) 35))))
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
