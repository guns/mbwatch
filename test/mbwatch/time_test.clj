(ns mbwatch.time-test
  (:require [clojure.test :refer [is]]
            [mbwatch.time :refer [dt->ms human-duration parse-ms]]
            [schema.test :refer [deftest]])
  (:import (org.joda.time DateTime)))

(deftest test-human-duration
  (is (= "0 seconds" (human-duration 0)))
  (is (= "1.0 second" (human-duration 1000)))
  (is (= "59.0 seconds" (human-duration 59000)))
  (is (= "5 hours, 1 minute, and 35.0 seconds"
         (human-duration (+ (* 5 60 60 1000) (* 1 60 1000) (* 35 1000) 1))))
  (let [dt (DateTime.)]
    (is (= "1.0 second" (human-duration dt (.plus dt 1000))))))

(deftest test-dt->ms
  (is (= 0 (dt->ms (DateTime. 0)))))

(deftest test-parse-ms
  (is (= 0 (parse-ms "")))
  (is (= (Math/round (+ (* 1.5 24 60 60 1000)
                        (* 1.0 60 60 1000)
                        (* 1.5 60 1000)
                        1000
                        1))
         (parse-ms "1.5d1h 1.5m1s\t1ms"))))
