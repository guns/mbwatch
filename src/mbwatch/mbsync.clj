(ns mbwatch.mbsync
  "The MbsyncMaster component takes an MbsyncCommand from a channel, then
   sends mail synchronization jobs to an MbsyncWorker via a UniqueBuffer
   channel, spawning a new worker if necessary. Each worker is responsible for
   syncing a single mbsync channel.

   The workers shell out to `mbsync`, passing a parsed configuration string
   via `bash -c 'mbsync -c <(cat)'`. These child processes can be terminated
   by MbsyncMaster at any time.

   Calling .stop on MbsyncMaster also stops all MbsyncWorkers.

       ─── MbsyncCommand ──┐
                           │
                           │
                           ▼
                    ┌──────────────┐              ─┐
                    │ MbsyncMaster │               │
                    └──────┬───────┘               │
                           │                       │
               ┌───────────┴──────────┐            ├── Loggable ───▶
               ▼                      ▼            │
        ┌──────────────┐       ┌──────────────┐    │
        │ MbsyncWorker │   …   │ MbsyncWorker │    │
        └──────────────┘       └──────────────┘   ─┘
   "
  (:require [clojure.core.async :refer [<!! >!! chan put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.config]
            [mbwatch.logging :refer [DEBUG ERR INFO Loggable NOTICE WARNING]]
            [mbwatch.process :as process :refer [dump! interruptible-wait]]
            [mbwatch.types :refer [->UniqueBuffer VOID]]
            [mbwatch.util :refer [class-name human-duration poison-chan
                                  shell-escape sig-notify-all thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [Int defschema either eq maybe
                                       protocol]]
            [schema.utils :refer [class-schema]])
  (:import (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.config Config)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:const ^:private CHAN_SIZE
  "TODO: Move to Config?"
  0x100)

(s/defn ^:private join-mbargs :- String
  [mbchan :- String
   mboxes :- [String]]
  (if (seq mboxes)
    (str mbchan \: (string/join \, mboxes))
    (str mbchan)))

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

(s/defrecord MbsyncEventStart
  [level  :- Int
   mbchan :- String
   mboxes :- [String]
   start  :- DateTime]

  Loggable

  (log-level [_] level)

  (->log [this]
    (let [{:keys [level mbchan mboxes start]} this
          msg (format "Starting `mbsync %s`" (join-mbargs mbchan mboxes))]
      (LogItem. level start msg))))

(s/defrecord MbsyncEventStop
  [level  :- Int
   mbchan :- String
   mboxes :- [String]
   start  :- DateTime
   stop   :- DateTime
   status :- Integer
   error  :- (maybe String)]

  Loggable

  (log-level [_] level)

  (->log [this]
    (let [{:keys [level mbchan mboxes start stop status error]} this
          mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (let [buf (StringBuffer.)
                      fail (if (<= level ERR)
                             (format "FAILURE: `mbsync %s` aborted in %s with status %d."
                                     mbarg Δt status)
                             (format "TERMINATED: `mbsync %s` terminated after %s with status %d."
                                     mbarg Δt status))]
                  (.append buf fail)
                  (when error
                    (.append buf \newline)
                    (.append buf error))
                  (str buf)))]
      (LogItem. level stop msg))))

(declare sync-boxes!)

(s/defrecord MbsyncWorker
  [mbsyncrc   :- String
   mbchan     :- String
   req-chan   :- ReadPort
   log-chan   :- WritePort
   monitor    :- AtomicBoolean
   state-chan :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (put! log-chan this)
    (assoc this :state-chan
           (thread-loop []
             (when (.get monitor)
               (with-chan-value [bs (<!! req-chan)]
                 (sync-boxes! this bs)
                 (recur))))))

  (stop [this]
    (put! log-chan this)
    (poison-chan req-chan state-chan)
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
   mboxes            :- [String]]
  (let [{:keys [mbsyncrc mbchan log-chan monitor]} mbsync-worker-map
        ev (strict-map->MbsyncEventStart
             {:level INFO
              :mbchan mbchan
              :mboxes mboxes
              :start (DateTime.)})
        proc (spawn-sync mbsyncrc mbchan mboxes)
        _ (put! log-chan ev)

        graceful? (interruptible-wait monitor proc)

        v (.exitValue proc)
        ev' (strict-map->MbsyncEventStop
              (assoc ev
                     :level (if graceful? (if (zero? v) NOTICE ERR) WARNING)
                     :stop (DateTime.)
                     :status v
                     :error (when (and graceful? (not (zero? v)))
                              (let [s (StringWriter.)]
                                (dump! proc :err s)
                                (str s)))))]
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
             (when-let [ws (handle-mbsync-command (<!! cmd-chan) workers this)]
               (recur ws)))))

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

(defschema MbsyncCommand
  (either {String [String]} ; Sync {mbchan [mbox]}
          (eq :term)        ; Terminate current sync processes
          (eq ::stop)       ; Terminate syncs and kill workers
          (eq nil)          ; Same as ::stop
          ))

(s/defn ^:private new-mbsync-worker :- MbsyncWorker
  [mbchan            :- String
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (let [{:keys [config log-chan]} mbsync-master-map]
    (map->MbsyncWorker
      {:mbsyncrc (:mbsyncrc config)
       :mbchan mbchan
       :req-chan (chan (->UniqueBuffer CHAN_SIZE))
       :log-chan log-chan
       :monitor (AtomicBoolean. true)})))

(s/defn ^:private dispatch-syncs :- {String MbsyncWorker}
  "Dispatch sync jobs to MbsyncWorker instances. Creates a new channel worker
   if it does not exist."
  [sync-req          :- {String [String]}
   workers           :- {String MbsyncWorker}
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (let [channels (-> mbsync-master-map :config :channels)]
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
  [command           :- MbsyncCommand
   workers           :- {String MbsyncWorker}
   mbsync-master-map :- (:schema (class-schema MbsyncMaster))]
  (case command
    nil    (do (stop-workers (vals workers)) nil)
    ::stop (do (stop-workers (vals workers)) nil)
    :term  (do (doseq [^MbsyncWorker w (vals workers)]
                 (sig-notify-all (.monitor w)))
               workers)
    (dispatch-syncs command workers mbsync-master-map)))
