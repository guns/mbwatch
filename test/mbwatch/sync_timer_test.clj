(ns mbwatch.sync-timer-test
  (:require [clojure.core.async :refer [<!! chan close!]]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [elements fmap one-of
                                                         return such-that
                                                         tuple]]
            [clojure.test.check.properties :refer [for-all]]
            [com.stuartsierra.component :as comp]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [update-timer!]]
            [mbwatch.sync-timer :as st])
  (:import (mbwatch.command Command)
           (mbwatch.sync_timer SyncTimer)))

(def mbox-gen
  (elements (mapv (comp str char) (range (int \α) (inc (int \ω))))))

(def mbchan-gen
  (elements (mapv str '[alpha beta gamma delta epsilon zeta eta theta iota
                        kappa lambda mu nu xi omicron pi rho sigma tau upsilon
                        phi chi psi omega])))

(def sync-request-gen
  (->> (tuple mbchan-gen (g/vector mbox-gen))
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

(def sync-timer-gen
  (->> (tuple sync-request-gen g/int)
       (fmap (fn [[req n]] (st/->SyncTimer req (chan 0x10000) n)))))

(defn chanv [ch]
  (into [] (take-while some? (repeatedly #(<!! ch)))))

(defn snapshot [sync-timer]
  (let [{:keys [sync-request-atom timer-atom]} sync-timer]
    {:sync-request-state @sync-request-atom
     :timer-state @timer-atom}))

(defspec test-cyclic-timer 10
  (for-all [sync-timer (such-that #(seq @(:sync-request-atom %)) sync-timer-gen)
            ttl (g/choose 100 1000)]
    (let [{:keys [cmd-chan-in cmd-chan-out log-chan sync-request-atom timer-atom]} sync-timer
          ;; Fire up to 1 + 20 sync requests
          _ (update-timer! timer-atom (quot ttl 20))
          sync-timer (comp/start sync-timer)]
      (Thread/sleep ttl)
      (close! cmd-chan-in)
      (comp/stop sync-timer)
      (let [cmds (chanv cmd-chan-out)
            n (count cmds)]
        (.println System/err (format "ttl %3d │ n %2d" ttl n))
        (and (is (every? (partial instance? Command) cmds))
             (is (every? #(= ((juxt :opcode :payload) %)
                             [:sync @sync-request-atom])
                         cmds))
             ;; Tolerate ±1 syncs (border conditions)
             (is (<= 19 n 21))
             ;; Lifecycle events only
             (is (= (mapv class (chanv log-chan)) [SyncTimer SyncTimer])))))))

; (defspec test-sync-timer-commands 100
;   (for-all [sync-timer sync-timer-gen
;             in-cmds (g/vector command-gen)]
;     (let [{:keys [cmd-chan-in cmd-chan-out log-chan]} sync-timer
;           state₀ (snapshot sync-timer)
;           ;; Spam the command channel
;           f (future
;               (doseq [cmd in-cmds]
;                 (>!! cmd-chan-in (with-meta cmd (snapshot sync-timer)))))
;           sync-timer (comp/start sync-timer)]
;       @f
;       (Thread/sleep 5) ; Settle down
;       (close! cmd-chan-in)
;       (comp/stop sync-timer)
;       (let [out-cmds (chanv cmd-chan-out)]
;         ;; Now re-play our input through a state machine:
;         (loop [state {}]
;           )))))
