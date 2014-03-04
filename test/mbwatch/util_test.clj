(ns mbwatch.util-test
  (:require [clojure.test :refer [deftest is]]
            [mbwatch.util :as u]))

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
