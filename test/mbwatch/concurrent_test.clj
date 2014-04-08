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
  (let [obj (Object.)
        p (promise)
        f (future (c/sig-wait obj) (deliver p obj))]
    (try
      (is (nil? (deref p 10 nil)))
      (c/sig-notify-all obj)
      (is (= obj (deref p 10 nil)))
      (finally
        (future-cancel f)))))

(deftest test-alarms
  (let [obj (Object.)
        t (System/currentTimeMillis)
        period (AtomicLong. 2000)
        alarm (AtomicLong. (+ t (.get period)))
        f (future (c/sig-wait-and-set-forward obj period alarm))]
    (Thread/sleep 10) ; Give the future thread a chance to start
    (c/reset-period-and-alarm 200 period alarm)
    (c/sig-notify-all obj)
    @f
    (is (< (- (System/currentTimeMillis) t) 300))
    (is (< 0 (- (.get alarm) (System/currentTimeMillis)) (inc (.get period))))))
