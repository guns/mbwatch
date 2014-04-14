(ns mbwatch.sync-timer-test
  (:require [clojure.core.async :refer [chan close!]]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [such-that]]
            [clojure.test.check.properties :refer [for-all]]
            [com.stuartsierra.component :as comp]
            [mbwatch.command]
            [mbwatch.concurrent :refer [update-timer!]]
            [mbwatch.sync-timer :refer [->SyncTimer]]
            [mbwatch.test.common :refer [chanv sync-request-gen]])
  (:import (mbwatch.command Command)
           (mbwatch.sync_timer SyncTimer)))

(defspec test-cyclic-timer 10
  (for-all [sync-req (such-that seq sync-request-gen)
            ttl (g/choose 100 1000)]
    (let [sync-timer (->SyncTimer sync-req (chan 0x10000) 0)
          {:keys [cmd-chan-in cmd-chan-out log-chan sync-request-atom timer-atom]} sync-timer
          ;; Fire up to 20 sync requests
          _ (update-timer! timer-atom (quot ttl 20) 0)
          sync-timer (comp/start sync-timer)]
      (Thread/sleep ttl)
      (close! cmd-chan-in)
      (comp/stop sync-timer)
      (let [cmds (chanv cmd-chan-out)
            n (count cmds)]
        (.println System/err (format "ttl %3d │ n %2d" ttl n))
        (and (is (every? #(and (instance? Command %)
                               (= ((juxt :opcode :payload) %)
                                  [:sync @sync-request-atom]))
                         cmds)
                 "only produces :sync Commands")
             ;; Tolerate ±1 syncs (border conditions)
             (is (<= 19 n 21) "cycles at a predictable rate")
             ;; Lifecycle events only
             (is (= (mapv class (chanv log-chan)) [SyncTimer SyncTimer])
                 "only logs itself"))))))

; (defn trigger?
;   "Command is either a :timer/trigger or a NOP :timer/set-period?"
;   [cur-period cmd]
;   (let [{:keys [opcode payload]} cmd]
;     (is (case opcode
;           :timer/trigger true
;           :timer/set-period (if (zero? cur-period)
;                               (< payload MIN-POS-PERIOD)
;                               (= payload cur-period))
;           false))))

; (defn cmd= [opcode payload command]
;   (is (= opcode (:opcode command)))
;   (is (= payload (zero-or-min (:payload command) MIN-POS-PERIOD))))

; (defspec test-sync-timer-commands 100
;   (for-all [sync-req (no-shrink sync-request-gen)
;             input-cmds (no-shrink (g/vector command-gen))]
;     (.println System/err "─────────────────────────────────────────────────────────────")
;     (let [sync-timer (->SyncTimer sync-req (chan) 0)
;           {:keys [cmd-chan-in cmd-chan-out log-chan sync-request-atom timer-atom]} sync-timer
;           sync-req-states (atom [@sync-request-atom])
;           timer-states (atom [@timer-atom])
;           _ (do (add-watch sync-request-atom ::watcher (fn [_ _ _ n] (swap! sync-req-states conj n)))
;                 (add-watch timer-atom ::watcher (fn [_ _ o n] (swap! timer-states conj n))))
;           ;; Pipe in our commands without spamming (multiple :timer/trigger
;           ;; commands would otherwise go unnoticed)
;           f (future (doseq [cmd input-cmds]
;                       (>!! cmd-chan-in cmd)
;                       (Thread/sleep 10)))
;           sync-timer (comp/start sync-timer)
;           ;; Settle down
;           _ (do @f
;                 (close! cmd-chan-in)
;                 (comp/stop sync-timer)
;                 (remove-watch sync-request-atom ::watcher)
;                 (remove-watch timer-atom ::watcher))
;           ;; Construct a hypothetical chain of command predicates given our
;           ;; state snapshots
;           preds (reduce
;                   (fn [v [o n]]
;                     (.println System/err [o '-> n])
;                     (cond (not= (:period o) (:period n)) (conj v (partial cmd= :timer/set-period (:period n)))
;                           :else (conj v (partial trigger? (:period n)))))
;                   [] (partition 2 1 @timer-states))]
;       (.println System/err (mapv (juxt :id :opcode :payload) input-cmds))
;       ;; Compare our expected commands against the actual input
;       (is (= (count preds) (count input-cmds)))
;       (is (every? true? (mapv (fn [p cmd] (p cmd)) preds input-cmds))))))
