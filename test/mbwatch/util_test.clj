(ns mbwatch.util-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.util :refer [catch-print chomp class-name dequote istr=
                                  parse-kv-string schema-params url-for
                                  zero-or-min]]
            [schema.test :refer [deftest]]))

(deftest test-catch-print
  (is (nil? (catch-print (throw (RuntimeException. "TESTING catch-print"))))))

(deftest test-parse-kv-string
  (is (= (parse-kv-string (slurp (io/resource "test-parse-kv-string.in")))
         {(keyword "α-α-α") "alpha, \"the first letter\""
          (keyword "β β β") "beta, 'the second letter'"
          (keyword "γ_γ\tγ") "gamma, #the ;third # letter"}))
  (is (thrown? RuntimeException (parse-kv-string "foo bar"))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (schema-params '[foo :- Int bar baz :- #{Symbol}])
         (schema-params '[foo bar baz]))))

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

(deftest test-class-name
  (is (= "String" (class-name ""))))

(deftest test-url-for
  (is (= "imaps://foo%40example.com@example.com:993"
         (url-for "imaps" "foo@example.com" "example.com" 993))))

(deftest test-zero-or-min
  (is (every? zero? (mapv #(zero-or-min % 0) [-1 0 Long/MIN_VALUE])))
  (is (every? #(= 5000 %) (mapv #(zero-or-min % 5000) [1 4999 5000]))))
