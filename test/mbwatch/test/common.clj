(ns mbwatch.test.common
  (:require [clojure.core.async :refer [<!!]]
            [clojure.test.check.generators :as g :refer [elements fmap]])
  (:import (java.io ByteArrayOutputStream File PrintStream)))

(defn set-of [gen]
  (fmap (partial into #{}) (g/vector gen)))

(def MBOX-GEN
  (elements (mapv (comp str char) (range (int \α) (inc (int \ω))))))

(def MBCHAN-GEN
  (elements (mapv str '[alpha beta gamma delta epsilon zeta eta theta iota
                        kappa lambda mu nu xi omicron pi rho sigma tau upsilon
                        phi chi psi omega])))

(def MBMAP-GEN
  (->> (g/tuple MBCHAN-GEN (set-of MBOX-GEN))
       g/vector
       (fmap (partial into {}))))

(defn chanv [ch]
  (into [] (take-while some? (repeatedly #(<!! ch)))))

(defn tol?
  "Is x within the expected deviation of k due to concurrent scheduling and
   test-time schema validation?"
  [k x]
  (<= -0.05 (double (/ (- x k) k)) 0.05))

(defmacro with-tempfile
  {:requires [File]}
  [tmp-sym & body]
  `(let [~tmp-sym (File/createTempFile "mbwatch_test" ".clj")]
     (try
       ~@body
       (finally
         (.delete ~tmp-sym)))))

(defmacro with-output
  {:requires [PrintStream ByteArrayOutputStream]}
  [& body]
  `(let [[out# err#] [System/out System/err]]
     (with-open [out-os# (PrintStream. (ByteArrayOutputStream.) true)
                 err-os# (PrintStream. (ByteArrayOutputStream.) true)]
       (try
         (System/setOut out-os#)
         (System/setErr err-os#)
         (let [v# (do ~@body)]
           [(str out-os#) (str err-os#) v#])
         (finally
           (System/setOut out#)
           (System/setErr err#))))))
