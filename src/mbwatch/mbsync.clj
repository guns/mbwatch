(ns mbwatch.mbsync
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.logging :refer [ERR ILogLevel INFO Loggable NOTICE
                                     WARNING]]
            [mbwatch.process :as process :refer [dump! interruptible-wait]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [class-name human-duration poison-chan
                                  shell-escape thread-loop with-chan-value]]
            [schema.core :as s :refer [Int defschema maybe protocol]]
            [schema.utils :refer [class-schema]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

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

  ILogLevel
  (log-level [_] level)

  Loggable
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

  ILogLevel
  (log-level [_] level)

  Loggable
  (->log [this]
    (let [{:keys [level mbchan mboxes start stop status error]} this
          mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (if (<= level ERR)
                  (format "FAILURE: `mbsync %s` aborted in %s with status %d.\n%s"
                          mbarg Δt status error)
                  (format "TERMINATED: `mbsync %s` terminated after %s with status %d.\n%s"
                          mbarg Δt status error)))]
      (LogItem. level stop msg))))

(declare sync-boxes!)

(s/defrecord MbsyncWorker
  [mbsyncrc :- String
   mbchan   :- String
   req-chan :- ReadPort
   log-chan :- WritePort
   monitor  :- Object]

  Lifecycle

  (start [this]
    (printf "↑ Starting %s for channel `%s`\n" (class-name this) mbchan)
    (assoc this ::worker
           (thread-loop []
             (with-chan-value [bs (<!! req-chan)]
               (sync-boxes! this bs)
               (recur)))))

  (stop [this]
    (printf "↓ Stopping %s for channel `%s`\n" (class-name this) mbchan)
    (poison-chan req-chan (::worker this))
    (dissoc this ::worker)))

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
                     :error (when-not (zero? v)
                              (with-out-str (dump! proc :err *out*)))))]
    (put! log-chan ev')))
