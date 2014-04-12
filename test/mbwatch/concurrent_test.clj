(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.concurrent :as c])
  (:import (java.util.concurrent.atomic AtomicLong)))

(defn tol?
  "Is Δt within the expected deviation due to concurrent scheduling?"
  [Δt]
  ;; This is a little large; _most_ runs deviate by less than 2ms
  (<= 0 Δt 6))

(defspec test-change-alarm 200
  (testing "changes in alarm time are detected and accounted for"
    (for-all [p₀ (g/such-that #(or (zero? %) (>= % 10)) g/nat)
              p₁ g/int
              p₂ (g/such-that #(>= % 10) g/nat)]
      (let [start (System/currentTimeMillis)
            period (AtomicLong. p₀)
            alarm (AtomicLong. (if (pos? p₀) (+ start p₀) 0))
            update? (promise)
            f (future
                ;; Wait for parent thread to enter wait
                (Thread/sleep 5)
                (deliver update? (c/update-period-and-alarm! p₁ period alarm))
                (when @update?
                  (c/sig-notify-all alarm))
                ;; Parent will wait indefinitely when alarm is 0, so notify
                ;; again in p₂ - 5 (ms we waited above)
                (when (zero? (.get alarm))
                  (Thread/sleep (- p₂ 5))
                  (c/sig-notify-all alarm)))
            _ (c/sig-wait-alarm alarm)
            stop (System/currentTimeMillis)
            _ (future-cancel f)
            Δt (- stop start)
            EΔt (if (pos? p₁)
                  (max 5 p₁)
                  p₂)
            dev (- Δt EΔt)]
        (.println System/err
                  (format "p₀ %4d │ p₁ %4d │ p₂ %4d │ update? %5s │ E[Δt] %4d │ Δt %4d │ dev %4d"
                          p₀ p₁ p₂ @update? EΔt Δt dev))
        (let [p (.get period)
              a (.get alarm)]
          (and (is (= @update? (not= p₀ (max p₁ 0))))
               (is (= p (max p₁ 0)))
               (if @update?
                 (if (pos? p₁)
                   (is (tol? (- stop a)))
                   (is (zero? a)))
                 (if (pos? p₀)
                   (is (= a (+ start p₀)))
                   (is (zero? a))))
               (is (tol? dev))))))))
