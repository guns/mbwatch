(ns mbwatch.mbsync
  "The MbsyncMaster component listens for :sync Commands on a channel, then
   sends mail synchronization jobs to MbsyncWorkers via buffered channels,
   spawning new workers if necessary. Each worker is responsible for syncing a
   single mbsync channel (mbchan).

   The workers shell out to `mbsync`, passing a parsed configuration string
   via `bash -c 'mbsync -c <(cat)'`. These child processes can be terminated
   by MbsyncMaster on receipt of a :term Command.

   Stopping the MbsyncMaster also stops all spawned MbsyncWorkers.

     ─────── Command ────────┐
                             │
                             ▼                      ─┐
                      ┌──────────────┐               │
                      │ MbsyncMaster │               │
                      └──────┬───────┘               │
                             │                       │
                 ┌───────────┴──────────┐            ├── Loggable ───▶
                 ▼                      ▼            │
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
            [mbwatch.logging :refer [->LogItem DEBUG ERR INFO Loggable NOTICE
                                     WARNING log!]]
            [mbwatch.mbsync.events :refer [join-mbargs
                                           strict-map->MbsyncEventStart
                                           strict-map->MbsyncEventStop]]
            [mbwatch.process :as process]
            [mbwatch.types :as t :refer [VOID]]
            [mbwatch.util :refer [shell-escape]]
            [schema.core :as s :refer [Int maybe protocol]])
  (:import (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.command Command)
           (mbwatch.config.mbsyncrc Mbsyncrc)
           (mbwatch.mbsync.events MbsyncUnknownChannelError)
           (org.joda.time DateTime)))

(s/defn ^:private spawn-sync :- Process
  "Asynchronously launch an mbsync process to sync a single mbchan. The config
   string is passed to mbsync via `cat` and bash's <(/dev/fd) feature in order
   to avoid temporary files."
  [rc     :- String
   mbchan :- String
   mboxes :- StringList]
  (process/spawn
    "bash" "-c" (str "exec mbsync -c <(cat) " (shell-escape (join-mbargs mbchan mboxes)))
    :in rc))

(declare sync-boxes!)

(t/defrecord MbsyncWorker
  [rc        :- String
   maildir   :- Maildirstore
   mbchan    :- String
   req-chan  :- ReadPort
   log-chan  :- WritePort
   status    :- AtomicBoolean
   exit-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (assoc this :exit-chan
           (thread-loop []
             (when (.get status)
               (when-some [req (<!! req-chan)]
                 (let [[id boxes] req]
                   (sync-boxes! this id boxes))
                 (recur))))))

  (stop [this]
    ;; Exit ASAP
    (.set status false)
    (log! log-chan this)
    (sig-notify-all status)
    (close! req-chan)
    (<!! exit-chan)
    (dissoc this :exit-chan))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s MbsyncWorker for channel `%s`"
                            (if exit-chan "↓ Stopping" "↑ Starting")
                            mbchan))))

(s/defn ^:private sync-boxes! :- VOID
  [mbsync-worker :- MbsyncWorker
   id            :- Int
   mboxes        :- StringList]
  (let [{:keys [rc maildir mbchan log-chan status]} mbsync-worker
        ev (strict-map->MbsyncEventStart
             {:level INFO
              :id id
              :mbchan mbchan
              :mboxes mboxes
              :start (DateTime.)})
        proc (spawn-sync rc mbchan mboxes)
        _ (put! log-chan ev)

        graceful? (process/interruptible-wait status proc)

        v (.exitValue proc)
        ev' (strict-map->MbsyncEventStop
              (assoc ev
                     :level (if graceful? (if (zero? v) NOTICE ERR) WARNING)
                     :stop (DateTime.)
                     :status v
                     :error (when (and graceful? (not (zero? v)))
                              (let [s (StringWriter.)]
                                (process/dump! proc :err s)
                                (str s)))
                     :maildir maildir))]
    (put! log-chan ev')
    nil))

(declare process-command)

(t/defrecord MbsyncMaster
  [mbsyncrc  :- Mbsyncrc
   cmd-chan  :- ReadPort
   log-chan  :- WritePort
   status    :- AtomicBoolean
   exit-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! log-chan this)
    (assoc this :exit-chan
           (thread-loop [workers {}]
             (let [cmd (when (.get status)
                         (<!! cmd-chan))]
               (when-let [workers (process-command this workers cmd)]
                 (recur workers))))))

  (stop [this]
    ;; Exit ASAP
    (.set status false)
    (log! log-chan this)
    (close! cmd-chan)
    (<!! exit-chan)
    (dissoc this :exit-chan))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-chan
                      "↓ Stopping MbsyncMaster"
                      "↑ Starting MbsyncMaster"))))

(s/defn ->MbsyncMaster :- MbsyncMaster
  [mbsyncrc :- Mbsyncrc
   cmd-chan :- ReadPort
   log-chan :- WritePort]
  (strict-map->MbsyncMaster
    {:mbsyncrc mbsyncrc
     :cmd-chan cmd-chan
     :log-chan log-chan
     :status (AtomicBoolean. true)
     :exit-chan nil}))

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
       :exit-chan nil})))

(s/defn ^:private dispatch-syncs :- {String MbsyncWorker}
  "Dispatch sync jobs to MbsyncWorker instances. Creates a new mbchan worker
   if it does not exist."
  [workers       :- {String MbsyncWorker}
   id            :- Int
   sync-req      :- {String StringList}
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
            (put! (:log-chan mbsync-master)
                  (MbsyncUnknownChannelError. id ch (DateTime.)))
            ws)))
      workers sync-req)))

(s/defn ^:private process-command :- (maybe {String MbsyncWorker})
  [mbsync-master :- MbsyncMaster
   workers       :- {String MbsyncWorker}
   cmd           :- (maybe Command)]
  (if cmd
    (case (:opcode cmd)
      :sync (do (put! (:log-chan mbsync-master) cmd)
                (dispatch-syncs workers
                                (:id cmd)
                                (:payload cmd)
                                mbsync-master))
      :term (do (doseq [w (vals workers)]
                  (sig-notify-all (:status w)))
                (put! (:log-chan mbsync-master) cmd)
                workers)
      workers)
    (dorun (pmap comp/stop (vals workers)))))
