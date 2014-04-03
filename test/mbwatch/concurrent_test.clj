(ns mbwatch.concurrent-test
  (:require [clojure.core.async :refer [<!! chan close! put! thread]]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [mbwatch.concurrent :as c]))

(deftest test-first-alt
  (is (= :first
         (c/first-alt (do (Thread/sleep 20) :second)
                      (do (Thread/sleep 10) :first))
         (c/first-alt :first :second :third))))

(deftest test-failsafe-pipe
  (let [from (chan 0x10000)
        to (chan 0x10000)
        sink (atom [])
        drain (fn [ch] (take-while some? (repeatedly #(<!! ch))))]
    (c/failsafe-pipe from to)
    (thread (reset! sink (drain to)))
    (dotimes [n 0xffff]
      (put! from n))
    (close! to)
    ;; Give the pipe a chance to save the orphaned element
    (Thread/sleep 10)
    (close! from)
    (is (empty? (set/difference (set (range 0xffff))
                                (set/union (set @sink) (set (drain from))))))))

(deftest test-concurrency-helpers
  (let [mon (Object.)
        p (promise)
        f (future (c/sig-wait mon) (deliver p 1))]
    (try
      (is (nil? (deref p 10 nil)))
      (c/sig-notify-all mon)
      (is (= (deref p 10 nil) 1))
      (finally
        (future-cancel f)))))
