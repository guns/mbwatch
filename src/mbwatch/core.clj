(ns mbwatch.core
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.config]
            [mbwatch.console-logger :refer [->ConsoleLogger]]
            [mbwatch.logging :refer [DEBUG map->LoggingService]]
            [mbwatch.mbsync :refer [map->MbsyncMaster]]
            [mbwatch.notification :refer [map->NewMessageNotificationService]]
            [schema.core :as s])
  (:import (mbwatch.config Config)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)))

(def ^:const ^:private CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

(s/defrecord Application
  [logging-service      :- LoggingService
   notification-service :- NewMessageNotificationService
   mbsync-master        :- MbsyncMaster]

  Lifecycle

  (start [this] (start-system this))
  (stop [this] (stop-system this)))

(s/defn new-application :- Application
  [config   :- Config
   cmd-chan :- ReadPort]
  (let [notify-chan (chan CHAN-SIZE)
        log-chan (chan CHAN-SIZE)]
    (Application.
      (map->LoggingService
        {:level DEBUG
         :logger (->ConsoleLogger System/out)
         :log-chan log-chan})
      (map->NewMessageNotificationService
        {:notify-cmd (-> config :mbwatchrc :notify-cmd)
         :notify-map-ref (atom {"self" #{"INBOX"}})
         :read-chan notify-chan
         :write-chan log-chan})
      (map->MbsyncMaster
        {:config config
         :cmd-chan cmd-chan
         :log-chan notify-chan}))))
