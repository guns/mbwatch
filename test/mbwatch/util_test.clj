(ns mbwatch.util-test
  (:require [clojure.core.async :refer [<!! chan put!]]
            [clojure.java.shell :refer [sh]]
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

(deftest test-concurrency-helpers
  (let [mon (Object.)
        p (promise)
        f (future (u/sig-wait mon) (deliver p 1))]
    (try
      (is (nil? (deref p 10 nil)))
      (u/sig-notify-all mon)
      (is (= (deref p 10 nil) 1))
      (finally
        (future-cancel f)))))

(deftest test-first-alt
  (is (= (u/first-alt (do (Thread/sleep 10) :first)
                      (do (Thread/sleep 20) :second))
         :first)))

(deftest test-core-async-helpers
  (let [ch (chan)
        vs (atom [])
        state-chan (u/thread-loop []
                     (u/with-chan-value [v (<!! ch)]
                       (swap! vs conj v)
                       (recur)))]
    (put! ch 0)
    (u/poison-chan ch state-chan)
    (put! ch 1)
    (is (= @vs [0]))))
