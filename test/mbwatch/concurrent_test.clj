(ns mbwatch.concurrent-test
  (:require [clojure.core.async :refer [<!! chan close! put! thread]]
            [clojure.set :as set]
            [clojure.test :refer [is]]
            [mbwatch.concurrent :as c]
            [schema.test :refer [deftest]]))

(deftest test-first-alt
  (is (= :first
         (c/first-alt (do (Thread/sleep 20) :second)
                      (do (Thread/sleep 10) :first))
         (c/first-alt :first :second :third))))

(deftest test-concurrency-helpers
  (let [mon (Object.)
        p (promise)
        f (future (c/sig-wait mon) (deliver p mon))]
    (try
      (is (nil? (deref p 10 nil)))
      (c/sig-notify-all mon)
      (is (= (deref p 10 nil) mon))
      (finally
        (future-cancel f)))))
