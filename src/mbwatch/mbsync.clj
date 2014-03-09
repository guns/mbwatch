(ns mbwatch.mbsync
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.process :as process :refer [dump!]]
            [mbwatch.util :refer [poison-chan shell-escape thread-loop
                                  with-chan-value]]
            [schema.core :as s :refer [maybe]])
  (:import (org.joda.time DateTime)))

(s/defn spawn-sync :- Process
  "Asynchronously launch an mbsync process to sync a single mail channel. The
   config string is passed to mbsync via `cat` and bash's <(/dev/fd) feature
   in order to avoid temporary files."
  [config :- String
   mbchan :- String
   mboxes :- [String]]
  (let [sync-arg (shell-escape
                   (if (seq mboxes)
                     (str mbchan \: (string/join \, mboxes))
                     (str mbchan)))]
    (process/spawn
      "bash" "-c" (str "exec mbsync -c <(cat) " sync-arg)
      :in config)))

(s/defrecord MbsyncEventStart
  [mbchan :- String
   mboxes :- [String]
   start  :- DateTime])

(s/defrecord MbsyncEventStop
  [mbchan :- String
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
                          {:mbchan mbchan :mboxes bs :start (DateTime.)})
                     proc (spawn-sync config mbchan bs)]
                 (put! log-chan ev)
                 (.waitFor proc)
                 (put! log-chan (strict-map->MbsyncEventStop
                                  (assoc ev
                                         :stop (DateTime.)
                                         :status (.exitValue proc)
                                         :error (when-not (zero? (.exitValue proc))
                                                  (with-out-str (dump! proc :err *out*)))))))
               (recur)))))

  (stop [this]
    (printf "↓ Stopping MbsyncWorker for channel `%s`\n" mbchan)
    (poison-chan req-chan (::worker this))
    (dissoc this ::worker)))
