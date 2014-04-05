(ns mbwatch.connection
  (:require [clojure.core.async :refer [<!! close!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.concurrent :refer [sig-notify-all sig-wait thread-loop]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable NOTICE WARNING
                                     log!]]
            [mbwatch.types :as t :refer [Word]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s :refer [Int maybe protocol]])
  (:import (clojure.lang Atom)
           (java.util.concurrent Future)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)))

(t/defrecord ^:private ConnectionEvent
  [mbchan :- String
   status :- Boolean]

  Loggable

  (log-level [_] (if status NOTICE WARNING))

  (log-item [this]
    (let [msg (str "Channel " mbchan " → " (if status "reachable" "unreachable"))]
      (->LogItem this msg))))

(s/defn update-connections! :- {String Boolean}
  [connection-atom        :- Atom
   mbchan->IMAPCredential :- {Word IMAPCredential}])

(t/defrecord ^:private ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   connection-atom         :- Atom ; {mbchan Boolean}
   poll-ms                 :- AtomicLong
   cmd-chan                :- ReadPort
   log-chan                :- WritePort
   next-check              :- AtomicLong
   status                  :- AtomicBoolean
   exit-future             :- (maybe Future)
   exit-chan               :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (add-watch connection-atom ::log-conn-changes
               (fn [_ _ o n]
                 (doseq [[mbchan conn] n
                         :let [oconn (o mbchan)]]
                   (when-not (= oconn conn)
                     (log! log-chan (ConnectionEvent. mbchan conn))))))
    (assoc this
           :exit-future
           (future
             (catch-print
               (loop []
                 (when (.get status)
                   ; (update-connections! connection-atom mbchan->IMAPCredential)
                   (let [poll (.get poll-ms)]
                     (.set next-check (+ (System/currentTimeMillis) poll))
                     (sig-wait status poll)
                     (recur))))))

           :exit-chan
           (thread-loop []
             (when (.get status)
               (when-some [cmd (<!! cmd-chan)]
                 (case (:opcode cmd)
                   :check-conn (sig-notify-all status)
                   nil)
                 (recur))))))

  (stop [this]
    ;; Exit ASAP
    (.set status false)
    (log! log-chan this)
    (sig-notify-all status)
    (close! cmd-chan)
    (remove-watch connection-atom ::log-conn-changes)
    @exit-future
    (<!! exit-chan)
    (dissoc this :exit-chan :exit-future))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-chan
                      "↓ Stopping ConnectionWatcher"
                      "↑ Starting ConnectionWatcher"))))

(s/defn ->ConnectionWatcher :- ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   connection-atom        :- Atom
   poll-ms                :- Int
   cmd-chan               :- ReadPort
   log-chan               :- WritePort]
  (strict-map->ConnectionWatcher
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :connection-atom connection-atom
     :poll-ms (AtomicLong. poll-ms)
     :cmd-chan cmd-chan
     :log-chan log-chan
     :next-check (AtomicLong. 0)
     :status (AtomicBoolean. true)
     :exit-future nil
     :exit-chan nil}))
