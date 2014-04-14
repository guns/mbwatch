(ns mbwatch.sync-timer-test
  (:require [clojure.core.async :refer [chan close!]]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [fmap such-that
                                                         tuple]]
            [clojure.test.check.properties :refer [for-all]]
            [com.stuartsierra.component :as comp]
            [mbwatch.command]
            [mbwatch.concurrent :refer [update-timer!]]
            [mbwatch.sync-timer :as st]
            [mbwatch.test.common :refer [chanv sync-request-gen]])
  (:import (mbwatch.command Command)
           (mbwatch.sync_timer SyncTimer)))

(def sync-timer-gen
  (->> (tuple sync-request-gen g/nat)
       (fmap (fn [[req n]] (st/->SyncTimer req (chan 0x10000) n)))))

; (defn =*
;   "Equal to, within tolerances."
;   [state state']
;   (if state'
;     (and (is (= (:sync-request state') (:sync-request state)))
;          (is (= (:period state') (:period state)))
;          (is (<= 0 (- (:alarm state') (:alarm state)) 10)))
;     (is (= state' state))))

(defspec test-cyclic-timer 10
  (for-all [sync-timer (such-that #(seq @(:sync-request-atom %)) sync-timer-gen)
            ttl (g/choose 100 1000)]
    (let [{:keys [cmd-chan-in cmd-chan-out log-chan sync-request-atom timer-atom]} sync-timer
          ;; Fire up to 1 + 20 sync requests
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
             (is (<= 20 n 22) "cycles at a predictable rate")
             ;; Lifecycle events only
             (is (= (mapv class (chanv log-chan)) [SyncTimer SyncTimer])
                 "only logs itself"))))))

; (defspec test-sync-timer-commands 100
;   (for-all [sync-timer sync-timer-gen
;             input-cmds (g/vector command-gen)]
;     (let [{:keys [cmd-chan-in cmd-chan-out log-chan sync-request-atom timer-atom]} sync-timer
;           sync-req-states (atom [@sync-request-atom])
;           timer-states (atom [@timer-atom])
;           _ (do (add-watch sync-request-atom ::watcher (fn [_ _ _ n] (swap! sync-req-states conj n)))
;                 (add-watch timer-atom ::watcher (fn [_ _ o n] (swap! timer-states conj n))))
;           ;; Spam the command channel
;           f (future (doseq [cmd input-cmds] (>!! cmd-chan-in cmd)))
;           sync-timer (comp/start sync-timer)
;           ;; Settle down
;           _ (do @f
;                 (Thread/sleep 10)
;                 (close! cmd-chan-in)
;                 (comp/stop sync-timer)
;                 (remove-watch sync-request-atom ::watcher)
;                 (remove-watch timer-atom ::watcher))
;           ;; Construct a hypothetical chain of commands given our state snapshots
;           _ (.println System/err "─────────────────────────────────────────────────────────────")
;           hyp (reduce
;                 (fn [v [o n]]
;                   (.println System/err [o '-> n])
;                   (cond (not= (:period o) (:period n)) (conj v [:timer/set-period (:period n)])
;                         (not= (:alarm o) (:alarm n)) (conj v [:timer/trigger nil])
;                         :else v))
;                 [] (partition 2 1 @timer-states))]
;       (.println System/err hyp)
;       (.println System/err (mapv (juxt :opcode :payload) input-cmds))
;       true)))
