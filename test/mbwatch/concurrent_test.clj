(ns mbwatch.concurrent-test
  (:require [clojure.core.async :refer [<!! chan put!]]
            [clojure.test :refer [deftest is]]
            [mbwatch.concurrent :as c]))

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

(deftest test-first-alt
  (is (= (c/first-alt (do (Thread/sleep 10) :first)
                      (do (Thread/sleep 20) :second))
         :first)))

(deftest test-core-async-helpers
  (let [ch (chan)
        vs (atom [])
        exit-chan (c/thread-loop []
                     (c/with-chan-value [v (<!! ch)]
                       (swap! vs conj v)
                       (recur)))]
    (put! ch 0)
    (c/poison-chan ch exit-chan)
    (put! ch 1)
    (is (= @vs [0]))))
