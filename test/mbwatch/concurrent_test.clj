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

(deftest test-concurrency-helpers
  (let [mon (Object.)
        p (promise)
        alarm (AtomicLong.)
        interval (AtomicLong. 5000)
        f (future (c/sig-wait mon) (deliver p mon))
        g (future (c/sig-wait-and-set-forward mon alarm interval))]
    (try
      (is (nil? (deref p 10 nil)))
      (c/sig-notify-all mon)
      (is (= (deref p 10 nil) mon))
      (is (< (System/currentTimeMillis) (.get alarm)))
      (finally
        (mapv future-cancel [f g])))))
