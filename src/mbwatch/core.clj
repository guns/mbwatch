(ns mbwatch.core
  "
     ─────── Command ────────┐
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
  (:require [clojure.core.async :refer [chan mult tap]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.concurrent :refer [CHAN-SIZE]]
            [mbwatch.config]
            [mbwatch.console-logger :refer [->ConsoleLogger
                                            MILLIS-TIMESTAMP-FORMAT
                                            get-default-colors]]
            [mbwatch.logging :refer [->LoggingService DEBUG]]
            [mbwatch.mbsync :refer [->MbsyncMaster]]
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (mbwatch.config Config)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)))

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
  (let [logger (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
        broadcast (mult cmd-chan)
        mbsync-chan (tap broadcast (chan CHAN-SIZE))
        log-chan (tap broadcast (chan CHAN-SIZE))
        notification-service (->NewMessageNotificationService
                               (-> config :mbwatchrc :notify-command)
                               (atom {"self" #{"INBOX"}}) ; FIXME: Move to config
                               log-chan)
        log-chan (:output-chan notification-service)]
    (Application.
      (->LoggingService DEBUG logger log-chan)
      notification-service
      (->MbsyncMaster (:mbsyncrc config) mbsync-chan log-chan))))
