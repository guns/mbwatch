(ns mbwatch.mbsync
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.process :as process :refer [dump!]]
            [mbwatch.types :refer [ERR INFO NOTICE]]
            [mbwatch.util :refer [poison-chan shell-escape thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [maybe]])
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
   start  :- DateTime])

(s/defrecord MbsyncEventStop
  [level  :- s/Int
   mbchan :- String
   mboxes :- [String]
   start  :- DateTime
   stop   :- DateTime
   status :- Integer
   error  :- (maybe String)])

(s/defrecord MbsyncWorker
  [config   :- String
   mbchan   :- String
   req-chan :- ReadPort
   log-chan :- WritePort]

  Lifecycle

  (start [this]
    (printf "↑ Starting MbsyncWorker for channel `%s`\n" mbchan)
    (assoc this ::worker
           (thread-loop []
             (with-chan-value [bs (<!! req-chan)]
               (let [ev (strict-map->MbsyncEventStart
                          {:level INFO
                           :mbchan mbchan
                           :mboxes bs
                           :start (DateTime.)})
                     proc (spawn-sync config mbchan bs)]
                 (put! log-chan ev)

                 (.waitFor proc)

                 (let [v (.exitValue proc)
                       ev' (strict-map->MbsyncEventStop
                             (assoc ev
                                    :level (if (zero? v) NOTICE ERR)
                                    :stop (DateTime.)
                                    :status v
                                    :error (when-not (zero? v)
                                             (with-out-str (dump! proc :err *out*)))))]
                   (put! log-chan ev')))
               (recur)))))

  (stop [this]
    (printf "↓ Stopping MbsyncWorker for channel `%s`\n" mbchan)
    (poison-chan req-chan (::worker this))
    (dissoc this ::worker)))
