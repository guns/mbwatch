(ns mbwatch.concurrent-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [such-that]]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.concurrent :as c :refer [->Timer]])
  (:import (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(defn tol?
  "Is x within the expected deviation of k due to concurrent scheduling?"
  [k x]
  (<= (double (/ (- k x) k)) 0.05))

(defspec test-simple-cyclic-timer 20
  (for-all [p (such-that #(>= % 5) g/nat)]
    (let [timer-atom (atom (->Timer p false))
          status (AtomicBoolean. true)
          E_i (quot 500 p)
          i (AtomicLong. 0)
          f (future (Thread/sleep 500) (.set status false))]
      (loop []
        (when (.get status)
          (c/sig-wait-timer timer-atom)
          (.set i (inc i))
          (c/set-alarm! timer-atom)
          (recur)))
      (.println System/err (format "p: %3d │ E[i]: %3d │ i: %3d │ ΔE[i]: %+.3f%%"
                                   p E_i (.get i) (double (/ (- E_i i) E_i))))
      (is (tol? E_i i)))))

(defspec test-change-alarm 200
  (for-all [p₀ (such-that #(or (zero? %) (>= % 10)) g/nat)
            p₁ g/int
            p₂ (such-that #(>= % 10) g/nat)]
    (let [start (System/currentTimeMillis)
          timer-atom (atom (->Timer p₀ false))
          update? (promise)
          f (future
              ;; Wait for parent thread to enter wait
              (Thread/sleep 5)
              (deliver update? (c/update-timer! timer-atom p₁))
              (when @update?
                (c/sig-notify-all timer-atom))
              ;; Parent will wait indefinitely when :alarm is 0, so notify
              ;; again in p₂ - 5 (ms we waited above)
              (when (zero? (:alarm @timer-atom))
                (Thread/sleep (- p₂ 5))
                (c/sig-notify-all timer-atom)))
          _ (c/sig-wait-timer timer-atom)
          stop (System/currentTimeMillis)
          _ (future-cancel f)
          Δt (- stop start)
          E_Δt (if (pos? p₁)
                (max 5 p₁)
                p₂)
          ΔE_Δt (- Δt E_Δt)]
      (.println System/err
                (format "p₀ %4d │ p₁ %4d │ p₂ %4d │ update? %5s │ E[Δt] %4d │ Δt %4d │ ΔE[Δt] %2d"
                        p₀ p₁ p₂ @update? E_Δt Δt ΔE_Δt))
      (let [{:keys [period alarm]} @timer-atom]
        (and (is (= @update? (not= p₀ (max p₁ 0))))
             (is (= period (max p₁ 0)))
             (if @update?
               (if (pos? p₁)
                 (is (<= 0 (- stop alarm) 1))
                 (is (zero? alarm)))
               (if (pos? p₀)
                 (is (<= 0 (- alarm (+ start p₀)) 1))
                 (is (zero? alarm))))
             (is (<= 0 ΔE_Δt 6)))))))
