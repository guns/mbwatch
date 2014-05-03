(ns mbwatch.concurrent-test
  (:require [clj-shellwords.core :refer [shell-escape]]
            [clojure.string :as string]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [such-that]]
            [clojure.test.check.properties :refer [for-all]]
            [mbwatch.concurrent :refer [->Timer pmapv set-alarm!
                                        shutdown-future sig-notify-all
                                        sig-wait sig-wait-timer
                                        synchronized-sh update-timer!]]
            [mbwatch.test.common :refer [tol? with-tempfile]]
            [schema.test :refer [deftest]])
  (:import (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(deftest test-shutdown-future
  (let [f₁ (future nil)
        f₂ (future (Thread/sleep 100))
        s₁ (future (shutdown-future f₁ 50))
        s₂ (future (shutdown-future f₂ 50))]
    (is (true? @s₁))
    (is (false? @s₂))))

(deftest test-pmapv
  (let [t₀ (System/currentTimeMillis)
        vs (pmapv (fn [n] (Thread/sleep 1000) n) (vec (range 256)))]
    (is (tol? 1000 (- (System/currentTimeMillis) t₀)))))

(deftest test-sig-fns
  (let [lock (Object.)
        f₁ (future (sig-wait lock) :f₁)
        f₂ (future (sig-wait lock 50) :f₂)]
    (Thread/sleep 100)
    (sig-notify-all lock)
    (is (= :f₁ @f₁))
    (is (= :f₂ @f₂))))

(defspec test-Timer-ctor 10000
  (for-all [p g/int
            m g/int
            b g/boolean]
    (let [start (System/currentTimeMillis)
          t (->Timer p m b)]
      (if (pos? p)
        (do (is (= (:period t) (max p m)) "period is at least min")
            (if b
              (is (<= 0 (- (:alarm t) start) 1) "alarm is set to go off immediately")
              (is (<= 0 (- (:alarm t) (+ start (:period t))) 1) "alarm is set +period")))
        (is (= 0 (:period t) (:alarm t)) "period and alarm are 0")))))

(defspec test-simple-cyclic-timer 10
  (for-all [p (such-that #(>= % 10) g/nat)]
    (let [timer-atom (atom (->Timer p 0 false))
          status (AtomicBoolean. true)
          E_i (quot 1000 p)
          i (AtomicLong. 0)
          f (future (Thread/sleep 1000) (.set status false))]
      (loop []
        (when (.get status)
          (sig-wait-timer timer-atom)
          (.set i (inc i))
          (set-alarm! timer-atom)
          (recur)))
      (.println System/err (format "p: %3d │ E[i]: %3d │ i: %3d │ ΔE[i]: %+.3f%%"
                                   p E_i (.get i) (double (/ (- i E_i) E_i))))
      (is (tol? E_i i) "performs like a simple sleep loop"))))

(defspec test-change-alarm 200
  (for-all [p₀ (such-that #(or (zero? %) (>= % 10)) g/nat)
            p₁ g/int
            p₂ (such-that #(>= % 10) g/nat)]
    (let [start (System/currentTimeMillis)
          timer-atom (atom (->Timer p₀ 0 false))
          update? (promise)
          f (future
              ;; Wait for parent thread to enter wait
              (Thread/sleep 5)
              (deliver update? (update-timer! timer-atom p₁ 0))
              (when @update?
                (sig-notify-all timer-atom))
              ;; Parent will wait indefinitely when :alarm is 0, so notify
              ;; again in p₂ - 5 (ms we waited above)
              (when (zero? (:alarm @timer-atom))
                (Thread/sleep (- p₂ 5))
                (sig-notify-all timer-atom)))
          _ (sig-wait-timer timer-atom)
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
        (and (is (= @update? (not= p₀ (max p₁ 0)))
                 "update-timer! returns true if value changed")
             (is (= period (max p₁ 0)) "period is not negative")
             (if @update?
               (if (pos? p₁)
                 (is (<= 0 (- stop alarm) 1) "woke up at alarm time")
                 (is (zero? alarm) "alarm started at zero"))
               (if (pos? p₀)
                 (is (<= 0 (- alarm (+ start p₀)) 1) "woke up at alarm time")
                 (is (zero? alarm) "alarm was set to zero")))
             (is (<= 0 ΔE_Δt 6) "test stops at expected time"))))))

(deftest test-synchronized-sh
  (with-tempfile tmp
    (let [path (shell-escape (str tmp))]
      (->> (vec (range 1000))
           (pmap (fn [_]
                   (synchronized-sh "sh" "-c" (str "date +%s%N >> " path))))
           doall)
      (is (apply < (mapv #(Long/parseLong %) (string/split-lines (slurp tmp))))))))
