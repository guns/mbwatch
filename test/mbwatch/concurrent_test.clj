(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.concurrent :as c])
  (:import (java.util.concurrent.atomic AtomicLong)))

(def init-period-gen (g/such-that #(>= % 10) g/pos-int))

(defspec test-change-alarm 100
  (testing "changes in alarm time are detected and accounted for"
    (for-all [p₀ init-period-gen
              p₁ g/int]
      (let [start (System/currentTimeMillis)
            period (AtomicLong. p₀)
            alarm (AtomicLong. (+ start p₀))
            update? (promise)
            f (future
                (Thread/sleep 5) ; Wait for parent thread to enter wait
                (deliver update? (c/update-period-and-alarm! p₁ period alarm))
                (when @update? (c/sig-notify-all alarm)))
            _ (c/sig-wait-alarm alarm)
            stop (System/currentTimeMillis)
            Δt (- stop start)
            EΔt (max p₁ 5)]
        (.println System/err (format "p₀: %3d, p₁: %3d, E[Δt]: %3d Δt: %3d"
                                     p₀ p₁ EΔt Δt))
        (and (is (= @update? (not= p₀ (max p₁ 0))))
             (is (= (.get period) (max p₁ 0)))
             (is (if @update?
                   (< (- stop (.get alarm)) 2)
                   (= (.get alarm) (+ start p₀))))
             (is (<= 0 (- Δt EΔt) 5)))))))
