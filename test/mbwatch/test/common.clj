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
  (one-of [(fmap #(->Command :sync %) MBMAP-GEN)
           (fmap #(->Command :sync/term %) (return nil))
           (fmap #(->Command :conn/trigger %) (return nil))
           (fmap #(->Command :conn/set-period %) g/int)
           (fmap #(->Command :conn/remove %) (set-of MBCHAN-GEN))
           (fmap #(->Command :idle/add %) MBMAP-GEN)
           (fmap #(->Command :idle/remove %) MBMAP-GEN)
           (fmap #(->Command :idle/set %) MBMAP-GEN)
           (fmap #(->Command :idle/restart %) (return nil))
           (fmap #(->Command :notify/add %) MBMAP-GEN)
           (fmap #(->Command :notify/remove %) MBMAP-GEN)
           (fmap #(->Command :notify/set %) MBMAP-GEN)
           (fmap #(->Command :timer/trigger %) (return nil))
           (fmap #(->Command :timer/set-period %) g/int)
           (fmap #(->Command :timer/set-request %) MBMAP-GEN)]))

(defn chanv [ch]
  (into [] (take-while some? (repeatedly #(<!! ch)))))

(defn tol?
  "Is x within the expected deviation of k due to concurrent scheduling and
   test-time schema validation?"
  [k x]
  (<= -0.05 (double (/ (- x k) k)) 0.05))
