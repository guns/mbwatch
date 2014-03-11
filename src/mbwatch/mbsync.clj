(ns mbwatch.mbsync
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.logging :refer [ERR ILogLevel INFO Loggable NOTICE
                                     WARNING]]
            [mbwatch.process :as process :refer [dump! interruptible-wait]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [human-duration poison-chan shell-escape
                                  thread-loop with-chan-value]]
            [schema.core :as s :refer [maybe]]
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
  [config :- String
   mbchan :- String
   mboxes :- [String]]
  (process/spawn
    "bash" "-c" (str "exec mbsync -c <(cat) " (shell-escape (join-mbargs mbchan mboxes)))
    :in config))

(s/defrecord MbsyncEventStart
  [level  :- s/Int
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
  [level  :- s/Int
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
  [config           :- String
   mbchan           :- String
   req-chan         :- ReadPort
   log-chan         :- WritePort
   interrupt-signal :- Object]

  Lifecycle

  (start [this]
    (printf "↑ Starting MbsyncWorker for channel `%s`\n" mbchan)
    (assoc this ::worker
           (thread-loop []
             (with-chan-value [bs (<!! req-chan)]
               (sync-boxes! this bs)
               (recur)))))

  (stop [this]
    (printf "↓ Stopping MbsyncWorker for channel `%s`\n" mbchan)
    (poison-chan req-chan (::worker this))
    (dissoc this ::worker)))

(s/defn ^:private sync-boxes! :- VOID
  [mbsync-worker-map :- (:schema (class-schema MbsyncWorker))
   mboxes            :- [String]]
  (let [{:keys [config mbchan req-chan log-chan interrupt-signal]} mbsync-worker-map
        ev (strict-map->MbsyncEventStart
             {:level INFO
              :mbchan mbchan
              :mboxes mboxes
              :start (DateTime.)})
        proc (spawn-sync config mbchan mboxes)
        _ (put! log-chan ev)

        graceful? (interruptible-wait interrupt-signal proc)

        v (.exitValue proc)
        ev' (strict-map->MbsyncEventStop
              (assoc ev
                     :level (if graceful? (if (zero? v) NOTICE ERR) WARNING)
                     :stop (DateTime.)
                     :status v
                     :error (when-not (zero? v)
                              (with-out-str (dump! proc :err *out*)))))]
    (put! log-chan ev')))
