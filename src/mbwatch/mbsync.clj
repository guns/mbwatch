(ns mbwatch.mbsync
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.logging :refer [ERR ILogLevel INFO NOTICE WARNING]]
            [mbwatch.process :as process :refer [dump! interruptible-wait]]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [poison-chan shell-escape thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [maybe]]
            [schema.utils :refer [class-schema]])
  (:import (org.joda.time DateTime)))

(s/defn join-mbargs :- String
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
  (log-level [_] level))

(s/defrecord MbsyncEventStop
  [level  :- s/Int
   mbchan :- String
   mboxes :- [String]
   start  :- DateTime
   stop   :- DateTime
   status :- Integer
   error  :- (maybe String)]

  ILogLevel
  (log-level [_] level))

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
