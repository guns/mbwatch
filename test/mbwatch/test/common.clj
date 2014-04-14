(ns mbwatch.test.common
  (:require [clojure.core.async :refer [<!!]]
            [clojure.test.check.generators :as g :refer [elements fmap one-of
                                                         return]]
            [mbwatch.command :refer [->Command]]))

(def mbox-gen
  (elements (mapv (comp str char) (range (int \α) (inc (int \ω))))))

(def mbchan-gen
  (elements (mapv str '[alpha beta gamma delta epsilon zeta eta theta iota
                        kappa lambda mu nu xi omicron pi rho sigma tau upsilon
                        phi chi psi omega])))

(def sync-request-gen
  (->> (g/tuple mbchan-gen (g/vector mbox-gen))
       g/vector
       (fmap (partial into {}))))

(def command-gen
  (one-of [(fmap #(->Command :sync %) sync-request-gen)
           (fmap #(->Command :sync/term %) (return nil))
           (fmap #(->Command :conn/trigger %) (return nil))
           (fmap #(->Command :conn/set-period %) g/int)
           (fmap #(->Command :conn/remove %) (g/vector mbchan-gen))
           (fmap #(->Command :notify/add %) sync-request-gen)
           (fmap #(->Command :notify/remove %) sync-request-gen)
           (fmap #(->Command :notify/set %) sync-request-gen)
           (fmap #(->Command :timer/trigger %) (return nil))
           (fmap #(->Command :timer/set-period %) g/int)
           (fmap #(->Command :timer/set-request %) sync-request-gen)]))

(defn chanv [ch]
  (into [] (take-while some? (repeatedly #(<!! ch)))))

(defn tol?
  "Is x within the expected deviation of k due to concurrent scheduling and
   test-time schema validation?"
  [k x]
  (<= -0.05 (double (/ (- x k) k)) 0.05))
