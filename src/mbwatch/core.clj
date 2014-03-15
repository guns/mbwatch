(ns mbwatch.core
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :refer [Channel]]
            [com.stuartsierra.component :refer [system-map]]
            [mbwatch.config]
            [mbwatch.console-logger :refer [->ConsoleLogger]]
            [mbwatch.logging :refer [DEBUG map->LoggingService]]
            [mbwatch.mbsync :refer [map->MbsyncMaster]]
            [schema.core :as s])
  (:import (com.stuartsierra.component SystemMap)
           (mbwatch.config Config)))

(def ^:private CHAN_SIZE
  "TODO: Move to Config?"
  0x100)

(s/defn new-system :- SystemMap
  [config   :- Config
   cmd-chan :- Channel]
  (let [log-chan (chan CHAN_SIZE)]
    (system-map
      :logger (map->LoggingService
                {:level DEBUG
                 :logger (->ConsoleLogger System/out)
                 :log-chan log-chan})
      :mbsync-master (map->MbsyncMaster
                       {:config config
                        :cmd-chan cmd-chan
                        :log-chan log-chan}))))
