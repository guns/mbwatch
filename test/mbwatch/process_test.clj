(ns mbwatch.process-test
  (:require [clojure.test :refer [is]]
            [mbwatch.process :as p]
            [schema.test :as s]))

(s/deftest test-spawn-and-dump!
  (is (= "Hello world.\n"
         (with-out-str
           (p/dump! (p/spawn "sh" "-c" "echo Hello world.") :out *out*))
         (with-out-str
           (p/dump! (p/spawn "sh" "-c" "echo Hello world. >&2") :err *out*))
         (with-out-str
           (p/dump! (p/spawn "cat" :in "Hello world.\n") :out *out*)))))
