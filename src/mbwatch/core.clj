(ns mbwatch.core
  "
     ─────── ICommand ───────┐
                             │
                             ▼                      ─┐
                      ┌──────────────┐               │
                      │ MbsyncMaster │               │
                      └──────┬───────┘               │
                             │                       │
                 ┌───────────┴──────────┐            ├── Loggable ──┐
                 ▼                      ▼            │              │
          ┌──────────────┐       ┌──────────────┐    │              │
          │ MbsyncWorker │   …   │ MbsyncWorker │    │              │
          └──────────────┘       └──────────────┘    │              │
                                                    ─┘              │
                                                                    │
                                                                    │
             ┌───────────────────────────────┐                      │
             │ NewMessageNotificationService │ ◀────────────────────┘
             └───────────────┬───────────────┘
                             │
                             │ Loggable
                             ▼
                     ┌────────────────┐
                     │ LoggingService │
                     └────────────────┘
  "
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.concurrent :refer [failsafe-pipe]]
            [mbwatch.config]
            [mbwatch.console-logger :refer [->ConsoleLogger
                                            MILLIS-TIMESTAMP-FORMAT
                                            get-default-colors]]
            [mbwatch.logging :refer [DEBUG strict-map->LoggingService]]
            [mbwatch.mbsync :refer [strict-map->MbsyncMaster]]
            [mbwatch.notification :refer [strict-map->NewMessageNotificationService]]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.config Config)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)))

(def ^:private ^:const CHAN-SIZE
  "TODO: Move to Config?"
  0x1000)

(t/defrecord ^:private Application
  [logging-service      :- LoggingService
   notification-service :- NewMessageNotificationService
   mbsync-master        :- MbsyncMaster]

  Lifecycle

  ;; While the application components do not explicitly depend on each other,
  ;; we do generally want the LogItem consumers to start before the producers
  ;; and stop after them. Here we depend on the implicit parameter-ordering of
  ;; (keys a-record) to start and stop the components in FILO order.
  (start [this] (start-system this))
  (stop [this] (stop-system this)))

(s/defn ->Application :- Application
  [config   :- Config
   cmd-chan :- ReadPort]
  (let [mbsync-cmd-chan (failsafe-pipe cmd-chan (chan CHAN-SIZE))
        notify-chan (chan CHAN-SIZE)
        log-chan (chan CHAN-SIZE)]
    (Application.
      (strict-map->LoggingService
        {:level DEBUG
         :logger (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
         :log-chan log-chan
         :exit-chan nil})
      (strict-map->NewMessageNotificationService
        {:notify-command (-> config :mbwatchrc :notify-command)
         :notify-map-ref (atom {"self" #{"INBOX"}})
         :read-chan notify-chan
         :write-chan log-chan
         :status (AtomicBoolean. true)
         :exit-chan nil})
      (strict-map->MbsyncMaster
        {:mbsyncrc (:mbsyncrc config)
         :cmd-chan mbsync-cmd-chan
         :log-chan notify-chan
         :status (AtomicBoolean. true)
         :exit-chan nil}))))
