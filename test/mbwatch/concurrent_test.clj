(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is]]
            [mbwatch.concurrent :as c]
            [schema.test :refer [deftest]])
  (:import (java.util.concurrent.atomic AtomicLong)))

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
        f (future (c/sig-wait-alarm lock alarm))]
    (Thread/sleep 10) ; Give the future thread a chance to start
    (is (true? (c/update-period-and-alarm! 200 period alarm)))
    (c/sig-notify-all lock)
    @f
    (is (< 150 (- (System/currentTimeMillis) t) 250))))
