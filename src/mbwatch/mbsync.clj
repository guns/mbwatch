(ns mbwatch.mbsync
  "The MbsyncMaster component listens for :sync Commands on a channel, then
   sends mail synchronization jobs to MbsyncWorkers via buffered channels,
   spawning new workers if necessary. Each worker is responsible for syncing a
   single mbsync channel (mbchan).

   The workers shell out to `mbsync`, passing a parsed configuration string
   via `bash -c 'mbsync -c <(cat)'`. These child processes can be terminated
   by MbsyncMaster on receipt of a :sync/term Command.

   Stopping the MbsyncMaster also stops all spawned MbsyncWorkers.

     ─────── Command ────────┐
                             │
                             ▼                      ─┐
                      ┌──────────────┐               │
                      │ MbsyncMaster │               │
                      └──────┬───────┘               │
                             │                       │
                  ┌──────────┴──────────┐            ├── Loggable ───▶
                  ▼                     ▼            │
          ┌──────────────┐       ┌──────────────┐    │
          │ MbsyncWorker │   …   │ MbsyncWorker │    │
          └──────────────┘       └──────────────┘    │
                                                    ─┘
   "
  (:require [clojure.core.async :refer [<!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.command]
            [mbwatch.concurrent :refer [CHAN-SIZE sig-notify-all thread-loop]]
            [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.events :refer [->MbsyncUnknownChannelError
                                    strict-map->MbsyncEventStart
                                    strict-map->MbsyncEventStop]]
            [mbwatch.logging :refer [->LogItem DEBUG ERR Loggable NOTICE
                                     WARNING log-with-timestamp!]]
            [mbwatch.process :as process]
            [mbwatch.shellwords :refer [shell-escape]]
            [mbwatch.types :as t :refer [MBMap VOID]]
            [mbwatch.util :refer [join-mbargs]]
            [schema.core :as s :refer [Int maybe]])
  (:import (clojure.lang IFn)
           (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.command Command)
           (mbwatch.config.mbsyncrc Mbsyncrc)
           (org.joda.time DateTime)))

(s/defn ^:private spawn-sync :- Process
  "Asynchronously launch an mbsync process to sync a single mbchan. The config
   string is passed to mbsync via `cat` and bash's <(/dev/fd) feature in order
   to avoid temporary files."
  [rc     :- String
   mbchan :- String
   mboxes :- #{String}]
  (process/spawn
    "bash" "-c" (str "exec mbsync -c <(cat) " (shell-escape (join-mbargs mbchan mboxes)))
    :in rc))

(declare sync-boxes!)

(t/defrecord ^:private MbsyncWorker
  [rc        :- String
   maildir   :- Maildirstore
   mbchan    :- String
   req-chan  :- ReadPort
   log-chan  :- WritePort
   status    :- AtomicBoolean
   exit-fn   :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [c (thread-loop []
              (when (.get status)
                (when-some [req (<!! req-chan)]
                  (apply sync-boxes! this req)
                  (recur))))]
      ;; MbsyncMaster will close the shared outgoing log-chan
      (assoc this :exit-fn
             #(do (.set status false)     ; Stop after current iteration
                  (sig-notify-all status) ; Terminate syncs
                  (<!! c)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s MbsyncWorker for channel `%s`"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            mbchan))))

(s/defn ^:private sync-boxes! :- VOID
  [mbsync-worker :- MbsyncWorker
   id            :- Int
   mboxes        :- #{String}]
  (let [{:keys [rc maildir mbchan log-chan status]} mbsync-worker
        ev (strict-map->MbsyncEventStart
             {:id id
              :mbchan mbchan
              :mboxes mboxes
              :start (DateTime.)})
        proc (spawn-sync rc mbchan mboxes)
        _ (put! log-chan ev)

        graceful? (process/interruptible-wait status proc)

        v (.exitValue proc)
        ev' (strict-map->MbsyncEventStop
              (assoc ev
                     :stop (DateTime.)
                     :level (if graceful? (if (zero? v) NOTICE ERR) WARNING)
                     :status v
                     :error (when (and graceful? (not (zero? v)))
                              (let [s (StringWriter.)]
                                (process/dump! proc :err s)
                                (str s)))
                     :maildir maildir))]
    (put! log-chan ev')
    nil))

(s/defn ^:private stop-workers! :- VOID
  [workers :- [MbsyncWorker]]
  (dorun
    (pmap #(do (close! (:req-chan %))
               (comp/stop %))
          workers)))

(declare process-command)

(t/defrecord ^:private MbsyncMaster
  [mbsyncrc  :- Mbsyncrc
   cmd-chan  :- ReadPort
   log-chan  :- WritePort
   status    :- AtomicBoolean
   exit-fn   :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [c (thread-loop [worker-map {}]
              (if-some [cmd (when (.get status) (<!! cmd-chan))]
                ;; We are the terminal consumer of commands, so log them
                (do (put! log-chan cmd)
                    (recur (process-command this worker-map cmd)))
                (stop-workers! (vals worker-map))))]
      (assoc this :exit-fn
             #(do (.set status false) ; Stop after current iteration
                  (<!! c)
                  (close! log-chan)   ; Close outgoing channels
                  ))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-fn
                      "↓ Stopping MbsyncMaster"
                      "↑ Starting MbsyncMaster"))))

(s/defn ->MbsyncMaster :- MbsyncMaster
  [mbsyncrc :- Mbsyncrc
   cmd-chan :- ReadPort]
  (strict-map->MbsyncMaster
    {:mbsyncrc mbsyncrc
     :cmd-chan cmd-chan
     :log-chan (chan CHAN-SIZE)
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private ->MbsyncWorker :- MbsyncWorker
  [mbchan        :- String
   mbsync-master :- MbsyncMaster]
  (let [{:keys [mbsyncrc log-chan]} mbsync-master]
    (strict-map->MbsyncWorker
      {:rc (-> mbsyncrc :text)
       :maildir (get-in mbsyncrc [:mbchan->Maildirstore mbchan])
       :mbchan mbchan
       :req-chan (chan CHAN-SIZE)
       :log-chan log-chan
       :status (AtomicBoolean. true)
       :exit-fn nil})))

(s/defn ^:private dispatch-syncs :- {String MbsyncWorker}
  "Dispatch sync jobs to MbsyncWorker instances. Creates a new mbchan worker
   if it does not exist."
  [worker-map    :- {String MbsyncWorker}
   id            :- Int
   sync-req      :- MBMap
   mbsync-master :- MbsyncMaster]
  (let [mbchans (-> mbsync-master :mbsyncrc :mbchans)]
    (reduce-kv
      (fn [ws ch bs]
        (if (contains? mbchans ch)
          (let [ws (if (contains? ws ch)
                     ws
                     (->> (->MbsyncWorker ch mbsync-master)
                          comp/start
                          (assoc ws ch)))]
            (put! (get-in ws [ch :req-chan]) [id bs])
            ws)
          (do
            (put! (:log-chan mbsync-master) (->MbsyncUnknownChannelError id ch))
            ws)))
      worker-map sync-req)))

(s/defn ^:private process-command :- {String MbsyncWorker}
  [mbsync-master :- MbsyncMaster
   worker-map    :- {String MbsyncWorker}
   cmd           :- (maybe Command)]
  (case (:opcode cmd)
    :sync (dispatch-syncs
            worker-map (:id cmd) (:payload cmd) mbsync-master)
    :sync/term (do (doseq [w (vals worker-map)]
                     (sig-notify-all (:status w)))
                   worker-map)
    worker-map))
