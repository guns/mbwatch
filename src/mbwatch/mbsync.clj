(ns mbwatch.mbsync
  "The MbsyncMaster component takes an ICommand from a channel, then sends
   mail synchronization jobs to an MbsyncWorker via a UniqueBuffer channel,
   spawning a new worker if necessary. Each worker is responsible for syncing
   a single mbsync channel.

   The workers shell out to `mbsync`, passing a parsed configuration string
   via `bash -c 'mbsync -c <(cat)'`. These child processes can be terminated
   by MbsyncMaster at any time.

   Calling .stop on MbsyncMaster also stops all MbsyncWorkers.

     ─────── ICommand ───────┐
                             │
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
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.logging :refer [DEBUG ERR INFO Loggable NOTICE WARNING]]
            [mbwatch.mbsync.events :refer [join-mbargs
                                           strict-map->MbsyncEventStart
                                           strict-map->MbsyncEventStop]]
            [mbwatch.process :as process]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [class-name poison-chan shell-escape
                                  thread-loop with-chan-value]]
            [schema.core :as s :refer [maybe protocol]]
            [schema.utils :refer [class-schema]])
  (:import (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:const ^:private CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

(s/defn spawn-sync :- Process
  "Asynchronously launch an mbsync process to sync a single mail channel. The
   config string is passed to mbsync via `cat` and bash's <(/dev/fd) feature
   in order to avoid temporary files."
  [mbsyncrc :- String
   mbchan   :- String
   mboxes   :- [String]]
  (process/spawn
    "bash" "-c" (str "exec mbsync -c <(cat) " (shell-escape (join-mbargs mbchan mboxes)))
    :in mbsyncrc))

(declare sync-boxes!)

(s/defrecord MbsyncWorker
  [mbsyncrc   :- String
   maildir    :- Maildirstore
   mbchan     :- String
   sync-chan  :- ReadPort
   log-chan   :- WritePort
   monitor    :- AtomicBoolean
   state-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (put! log-chan this)
    (assoc this :state-chan
           (thread-loop []
             (when (.get monitor)
               (with-chan-value [bs (<!! sync-chan)]
                 (apply sync-boxes! this)
                 (recur))))))

  (stop [this]
    (put! log-chan this)
    (poison-chan sync-chan state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s %s for channel `%s`"
                      (if state-chan "↓ Stopping" "↑ Starting")
                      (class-name this)
                      mbchan)]
      (LogItem. DEBUG (DateTime.) msg))))

(s/defn ^:private sync-boxes! :- VOID
  [mbsync-worker-map :- (:schema (class-schema MbsyncWorker))
   id                :- Long
   mboxes            :- [String]]
  (let [{:keys [mbsyncrc maildir mbchan log-chan monitor]} mbsync-worker-map
        ev (strict-map->MbsyncEventStart
             {:level INFO
              :id id
              :mbchan mbchan
              :mboxes mboxes
              :start (DateTime.)})
        proc (spawn-sync mbsyncrc mbchan mboxes)
        _ (put! log-chan ev)

        graceful? (process/interruptible-wait monitor proc)

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

(declare handle-mbsync-command)

(s/defrecord MbsyncMaster
  [config     :- Config
   cmd-chan   :- ReadPort
   log-chan   :- WritePort
   state-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (put! log-chan this)
    (assoc this :state-chan
           (thread-loop [workers {}]
             (let [cmd (<!! cmd-chan)]
               (when cmd
                 (put! log-chan cmd))
               (when-let [ws (handle-mbsync-command cmd workers this)]
                 (recur ws))))))

  (stop [this]
    (put! log-chan this)
    (>!! cmd-chan ::stop)
    (<!! state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (LogItem. DEBUG (DateTime.) (str (if state-chan "↓ Stopping " "↑ Starting ")
                                     (class-name this)))))

(s/defn ^:private stop-workers :- [MbsyncWorker]
  [workers :- [MbsyncWorker]]
  (doall
    (pmap (fn [^MbsyncWorker w]
            (let [mon ^AtomicBoolean (.monitor w)]
              (.set mon false)
              (sig-notify-all mon)
              (.stop w)))
          workers)))

(s/defn ^:private new-mbsync-worker :- MbsyncWorker
  [mbchan            :- String
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (let [{:keys [config log-chan]} mbsync-master-map]
    (map->MbsyncWorker
      {:mbsyncrc (-> config :mbsyncrc :text)
       :maildir (get-in config [:mbsyncrc :channels->maildirstores mbchan])
       :mbchan mbchan
       :req-chan (chan (->UniqueBuffer CHAN-SIZE))
       :log-chan log-chan
       :monitor (AtomicBoolean. true)})))

(s/defn ^:private dispatch-syncs :- {String MbsyncWorker}
  "Dispatch sync jobs to MbsyncWorker instances. Creates a new channel worker
   if it does not exist."
  [sync-req          :- {String [String]}
   workers           :- {String MbsyncWorker}
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (let [channels (-> mbsync-master-map :config :mbsyncrc :channels)]
    (reduce-kv
      (fn [ws ch bs]
        (if (contains? channels ch)
          (let [ws (if (contains? ws ch)
                     ws
                     (assoc ws ch (.start (new-mbsync-worker ch mbsync-master-map))))]
            (put! (get-in ws [ch :req-chan]) bs)
            ws)
          (do (put! (:log-chan mbsync-master-map)
                    (LogItem. WARNING (DateTime.) (format "Unknown channel: `%s`" ch)))
              ws)))
      workers sync-req)))

(s/defn ^:private handle-mbsync-command :- (maybe {String MbsyncWorker})
  [mbsync-command    :- MbsyncCommand
   workers           :- {String MbsyncWorker}
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (let [{:keys [command]} mbsync-command]
    (case command
      nil        (do (stop-workers (vals workers)) nil)
      ::stop     (do (stop-workers (vals workers)) nil)
      :terminate (do (doseq [^MbsyncWorker w (vals workers)]
                       (sig-notify-all (.monitor w)))
                     workers)
      (dispatch-syncs command workers mbsync-master-map))))
