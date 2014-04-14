(ns mbwatch.process-test
  (:require [clojure.test :refer [is testing]]
            [mbwatch.concurrent :refer [sig-notify-all]]
            [mbwatch.process :refer [dump! interruptible-wait spawn]]
            [schema.test :refer [deftest]]))

(deftest test-spawn-and-dump!
  (is (= "Hello world.\n"
         (with-out-str
           (dump! (spawn "sh" "-c" "echo Hello world.") :out *out*))
         (with-out-str
           (dump! (spawn "sh" "-c" "echo Hello world. >&2") :err *out*))
         (with-out-str
           (dump! (spawn "cat" :in "Hello world.\n") :out *out*))))
  (testing "Should not throw IOException"
    (let [proc (spawn "sh" "-c" "echo Hello world.")]
      (.close (.getInputStream proc))
      (is (= "" (with-out-str (dump! proc :out *out*)))))))

(deftest test-interruptible-wait
  (is (true? (interruptible-wait (Object.) (spawn "true"))))
  (is (false?
        (let [lock (Object.)
              f (future (interruptible-wait lock (spawn "sleep" "1")))]
          (Thread/sleep 10) ; Give the future thread a chance to start
          (sig-notify-all lock)
          @f))))
