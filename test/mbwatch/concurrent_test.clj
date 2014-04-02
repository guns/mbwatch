(ns mbwatch.concurrent-test
  (:require [clojure.core.async :refer [<!! chan put!]]
            [clojure.test :refer [deftest is]]
            [mbwatch.concurrent :as c]))

(deftest test-first-alt
  (is (= (c/first-alt (do (Thread/sleep 10) :first)
                      (do (Thread/sleep 20) :second))
         :first)))

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
