(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.concurrent :as c])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defspec test-alarms 100
  (for-all [p1 (g/such-that #(>= % 10) g/pos-int)
            p2 g/pos-int]
    (let [start (System/currentTimeMillis)
          period (AtomicLong. p1)
          alarm (AtomicLong. (+ start p1))
          p (promise)
          f (future
              (Thread/sleep 5) ; Wait for parent thread to enter wait
              (deliver p (c/update-period-and-alarm! p2 period alarm))
              (when @p (c/sig-notify-all alarm)))
          _ (c/sig-wait-alarm alarm)
          stop (System/currentTimeMillis)]
      (.println System/err (format "p1: %3d, p2: %3d, min: %3d, time: %3d"
                                   p1 p2 (max p2 5) (- stop start)))
      (and (is (= @p (not= p1 p2)))
           (is (.get period) p2)
           (is (.get alarm) (+ start p2))
           (is (<= 0 (- stop start (max p2 5)) 5))))))
