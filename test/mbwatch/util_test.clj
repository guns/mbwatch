(ns mbwatch.util-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.util :refer [chomp class-name dequote istr= make-table
                                  multi-row-entry parse-kv-string
                                  parse-pattern url-for when-seq zero-or-min]]
            [schema.test :refer [deftest]]))

(deftest test-macros
  (is (nil? (when-seq [x []] x)))
  (is (= [1] (when-seq [x [1]] x))))

(deftest test-tables
  (is (= (make-table ["name" "address" "number"]
                     [["Alice" "1234 Main St." "1234"]
                      ["Bob" "1236 Main St." "4321"]
                      ["Carol" "SCREW YOU" "XXXX"]])
         (str "name  | address       | number\n"
              "------+---------------+-------\n"
              "Alice | 1234 Main St. | 1234  \n"
              "Bob   | 1236 Main St. | 4321  \n"
              "Carol | SCREW YOU     | XXXX  ")))
  (is (= (multi-row-entry "Foo" ["bar" "baz"] ["Alice" "Bob"])
         [["Foo" "bar" "Alice"] ["" "baz" "Bob"]])))

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

(deftest test-parse-pattern
  (is (= (str #"\b\Qfoo\bar\E\b") (str (parse-pattern "foo\\bar"))))
  (is (= (str #"(?i)\bfoo\\bar\b|baz") (str (parse-pattern "/(?i)\\bfoo\\\\bar\\b|baz/"))))
  (is (= (str #"\A.*(\p{Alpha}|/)\z")
         (str (parse-pattern "/\\A.*(\\p{Alpha}|\\/)\\z/")))))

(deftest test-class-name
  (is (= "String" (class-name ""))))

(deftest test-url-for
  (is (= "imaps://foo%40example.com@example.com:993"
         (url-for "imaps" "foo@example.com" "example.com" 993))))

(deftest test-zero-or-min
  (is (every? zero? (mapv #(zero-or-min % 0) [-1 0 Long/MIN_VALUE])))
  (is (every? #(= 5000 %) (mapv #(zero-or-min % 5000) [1 4999 5000]))))
