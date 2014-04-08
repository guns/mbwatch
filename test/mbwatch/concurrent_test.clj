(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is]]
            [mbwatch.concurrent :as c]
            [schema.test :refer [deftest]])
  (:import (java.util.concurrent.atomic AtomicLong)))

(deftest test-first-alt
  (is (= :first
         (c/first-alt (do (Thread/sleep 20) :second)
                      (do (Thread/sleep 10) :first))
         (c/first-alt :first :second :third))))

(deftest test-wait-and-notify
  (let [lock (Object.)
        p (promise)
        f (future (c/sig-wait lock) (deliver p lock))]
    (try
      (is (nil? (deref p 10 nil)))
      (c/sig-notify-all lock)
      (is (= lock (deref p 10 nil)))
      (finally
        (future-cancel f)))))

(deftest test-alarms
  (let [lock (Object.)
        t (System/currentTimeMillis)
        period (AtomicLong. 2000)
        alarm (AtomicLong. (+ t (.get period)))
        f (future (c/sig-wait-and-set-forward lock period alarm))]
    (Thread/sleep 10) ; Give the future thread a chance to start
    (c/reset-period-and-alarm 200 period alarm)
    (c/sig-notify-all lock)
    @f
    (is (< (- (System/currentTimeMillis) t) 300))
    (is (< 0 (- (.get alarm) (System/currentTimeMillis)) (inc (.get period))))))
