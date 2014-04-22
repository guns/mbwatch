(ns mbwatch.sync-timer-test
  (:require [clojure.core.async :refer [chan close!]]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as g :refer [such-that]]
            [clojure.test.check.properties :refer [for-all]]
            [com.stuartsierra.component :as comp]
            [mbwatch.command :refer [CommandSchema]]
            [mbwatch.concurrent :refer [update-timer!]]
            [mbwatch.sync-timer :refer [->SyncTimer]]
            [mbwatch.test.common :refer [MBMAP-GEN chanv]]
            [schema.core :refer [validate]])
  (:import (mbwatch.sync_timer SyncTimer)))

(defspec test-cyclic-timer 10
  (for-all [sync-req (such-that seq MBMAP-GEN)
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
        (and (is (every? #(and (validate CommandSchema %)
                               (= ((juxt :opcode :payload) %)
                                  [:sync @sync-request-atom]))
                         cmds)
                 "only produces :sync Commands")
             ;; Tolerate ±1 syncs (border conditions)
             (is (<= 19 n 21) "cycles at a predictable rate")
             ;; Lifecycle events only
             (is (= (mapv class (chanv log-chan)) [SyncTimer SyncTimer])
                 "only logs itself"))))))
