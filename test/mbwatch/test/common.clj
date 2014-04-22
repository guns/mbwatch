(ns mbwatch.test.common
  (:require [clojure.core.async :refer [<!!]]
            [clojure.test.check.generators :as g :refer [elements fmap one-of
                                                         return]]
            [mbwatch.command :refer [->Command]]))

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

(def COMMAND-GEN
  ;; FIXME
  )

(defn chanv [ch]
  (into [] (take-while some? (repeatedly #(<!! ch)))))

(defn tol?
  "Is x within the expected deviation of k due to concurrent scheduling and
   test-time schema validation?"
  [k x]
  (<= -0.05 (double (/ (- x k) k)) 0.05))
