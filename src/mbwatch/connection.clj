(ns mbwatch.connection
  (:require [clojure.core.async :refer [<!! close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command]
            [mbwatch.concurrent :refer [future-catch-print sig-notify-all
                                        sig-wait thread-loop]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable NOTICE
                                     WARNING log!]]
            [mbwatch.network :refer [reachable?]]
            [mbwatch.types :as t :refer [VOID Word]]
            [schema.core :as s :refer [Int maybe protocol]]
            [schema.utils :refer [class-schema]])
  (:import (clojure.lang Atom IFn)
           (java.util.concurrent Future)
           (java.util.concurrent.atomic AtomicBoolean AtomicLong)
           (mbwatch.command Command)
           (org.joda.time DateTime)))

(t/defrecord ConnectionEvent
  [mbchan    :- String
   status    :- (maybe Boolean)
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (case status
      true  NOTICE
      false WARNING
      nil   INFO))

  (log-item [this]
    (let [msg (str "Channel " mbchan (case status
                                       true  " → reachable"
                                       false " ✖ unreachable"
                                       nil   " ∅ unregistered"))]
      (->LogItem this msg))))

(s/defn ^:private update-conn-map :- {String Boolean}
  "Update connection-map by checking connections in parallel. mbchans not
   present in mbchan->IMAPCredential are removed."
  [connection-map         :- {String Boolean}
   mbchan->IMAPCredential :- {Word IMAPCredential}
   timeout                :- Int]
  (->> (keys connection-map)
       (pmap (fn [mbchan]
               (when-let [imap (mbchan->IMAPCredential mbchan)]
                 [mbchan (reachable? (:host imap) (:port imap) timeout)])))
       (remove nil?)
       (into {})))

(s/defn ^:private log-conn-changes-fn :- IFn
  [log-chan]
  (fn [_ _ old-map new-map]
    ;; Statuses were swapped in atomically, so don't mislead the user
    (let [dt (DateTime.)]
      (doseq [mbchan (distinct (mapcat keys [old-map new-map]))]
        (if-some [conn (new-map mbchan)]
          (when-not (= (old-map mbchan) conn)
            (put! log-chan (ConnectionEvent. mbchan conn dt)))
          (put! log-chan (ConnectionEvent. mbchan nil dt)))))))

(declare update-conn-and-wait!)
(declare process-command)

(t/defrecord ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   connection-atom        :- Atom ; {mbchan Boolean}
   poll-ms                :- AtomicLong
   cmd-chan               :- ReadPort
   log-chan               :- WritePort
   next-check             :- AtomicLong
   status                 :- AtomicBoolean
   exit-future            :- (maybe Future)
   exit-chan              :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (add-watch connection-atom ::log-conn-changes
               (log-conn-changes-fn log-chan))
    (assoc this
           :exit-future
           (future-catch-print
             (loop []
               (when (.get status)
                 (update-conn-and-wait! this)
                 (recur))))
           :exit-chan
           (thread-loop []
             (when (.get status)
               (when-some [cmd (<!! cmd-chan)]
                 (process-command this cmd)
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

(s/defn ^:private update-conn-and-wait! :- VOID
  [connection-watcher :- (class-schema ConnectionWatcher)]
  (let [{:keys [connection-atom
                mbchan->IMAPCredential
                ^AtomicLong poll-ms
                ^AtomicLong next-check
                status]} connection-watcher]
    (swap! connection-atom #(update-conn-map % mbchan->IMAPCredential 2000)) ; FIXME: Move to config
    (let [poll (.get poll-ms)]
      (.set next-check (+ (System/currentTimeMillis) poll))
      (sig-wait status poll))))

(s/defn ^:private process-command :- VOID
  [connection-watcher :- (class-schema ConnectionWatcher)
   command            :- Command]
  (case (:opcode command)
    :check-conn (sig-notify-all (:status connection-watcher))
    nil))

(s/defn ->ConnectionWatcher :- ConnectionWatcher
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   poll-ms                :- Int
   cmd-chan               :- ReadPort
   log-chan               :- WritePort]
  (strict-map->ConnectionWatcher
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :connection-atom (atom {})
     :poll-ms (AtomicLong. poll-ms)
     :cmd-chan cmd-chan
     :log-chan log-chan
     :next-check (AtomicLong. 0)
     :status (AtomicBoolean. true)
     :exit-future nil
     :exit-chan nil}))
