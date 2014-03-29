(ns mbwatch.process-test
  (:require [clojure.test :refer [is testing]]
            [mbwatch.process :as p]
            [mbwatch.util :refer [sig-notify-all]]
            [schema.test :as s]))

(s/deftest test-spawn-and-dump!
  (is (= "Hello world.\n"
         (with-out-str
           (p/dump! (p/spawn "sh" "-c" "echo Hello world.") :out *out*))
         (with-out-str
           (p/dump! (p/spawn "sh" "-c" "echo Hello world. >&2") :err *out*))
         (with-out-str
           (p/dump! (p/spawn "cat" :in "Hello world.\n") :out *out*))))
  (testing "Should not throw IOException"
    (let [proc (p/spawn "sh" "-c" "echo Hello world.")]
      (.close (.getInputStream proc))
      (is (= "" (with-out-str (p/dump! proc :out *out*)))))))

(s/deftest test-interruptible-wait
  (is (true? (p/interruptible-wait (Object.) (p/spawn "true"))))
  (is (false?
        (let [mon (Object.)
              f (future (p/interruptible-wait mon (p/spawn "sleep" "1")))]
          (Thread/sleep 10) ; Give the future thread a chance to start
          (sig-notify-all mon)
          @f))))
