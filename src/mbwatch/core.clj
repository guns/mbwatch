(ns mbwatch.core
  "
       ──── Command ─────────┐
                             │ 0
                             ▼                     ──┐
                       ┌───────────┐                 │
                       │ SyncTimer │                 │
                       └─────┬─────┘                 │
                             │                       │
                             │ 1                     │
                             ▼                       │
                   ┌───────────────────┐             │
                   │ ConnectionWatcher │             │
                   └─────────┬─────────┘             │
                             │                       │
                             │ 2                     ├── Loggable ──┐
                             ▼                       │              │
                      ┌──────────────┐               │              │
                      │ MbsyncMaster │               │              │
                      └──────┬───────┘               │              │
                             │                       │              │
                 ┌───────────┴──────────┐            │              │
                 ▼                      ▼            │              │
          ┌──────────────┐       ┌──────────────┐    │              │ 0
          │ MbsyncWorker │   …   │ MbsyncWorker │    │              │
          └──────────────┘       └──────────────┘    │              │
                                                   ──┘              │
                                                                    │
                                                                    │
             ┌───────────────────────────────┐                      │
             │ NewMessageNotificationService │ ◀────────────────────┘
             └───────────────┬───────────────┘
                             │
                             │ 1
                             ▼
                     ┌────────────────┐
                     │ LoggingService │
                     └────────────────┘
  "
  (:require [clojure.core.async :refer [chan]]
            [clojure.core.async.impl.protocols :refer [WritePort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.concurrent :refer [CHAN-SIZE]]
            [mbwatch.config]
            [mbwatch.connection-watcher :refer [->ConnectionWatcher]]
            [mbwatch.console-logger :refer [->ConsoleLogger
                                            MILLIS-TIMESTAMP-FORMAT
                                            get-default-colors]]
            [mbwatch.logging :refer [->LoggingService DEBUG]]
            [mbwatch.mbsync :refer [->MbsyncMaster]]
            [mbwatch.mbsync.sync-timer :refer [->SyncTimer]]
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (mbwatch.config Config)
           (mbwatch.connection_watcher ConnectionWatcher)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.mbsync.sync_timer SyncTimer)
           (mbwatch.notification NewMessageNotificationService)))

(t/defrecord Application
  [cmd-chan             :- WritePort
   logging-service      :- LoggingService
   notification-service :- NewMessageNotificationService
   mbsync-master        :- MbsyncMaster
   connection-watcher   :- ConnectionWatcher
   sync-timer           :- SyncTimer]

  Lifecycle

  ;; While the application components do not explicitly depend on each other,
  ;; we do generally want the LogItem consumers to start before the producers
  ;; and stop after them. Here we depend on the implicit parameter-ordering of
  ;; (keys a-record) to start and stop the components in FILO order.
  (start [this] (start-system this (rest (keys this))))
  (stop [this] (stop-system this (rest (keys this)))))

(s/defn ->Application :- Application
  [config :- Config]
  (let [;; Top level log consumer
        notification-service (->NewMessageNotificationService
                               (-> config :mbwatchrc :notify-command)
                               {"self" #{"INBOX"}} ; FIXME: Move to config
                               (chan CHAN-SIZE))
        log-chan-0 (:log-chan-in notification-service)
        log-chan-1 (:log-chan-out notification-service)
        ;; Top level cmd consumer
        sync-timer (->SyncTimer
                     {} ; FIXME: Move to config
                     (chan CHAN-SIZE)
                     log-chan-0
                     (* 15 60 1000)) ; FIXME: Move to config
        cmd-chan-0 (:cmd-chan-in sync-timer)
        cmd-chan-1 (:cmd-chan-out sync-timer)
        ;; Middleware
        connection-watcher (->ConnectionWatcher
                             (-> config :mbsyncrc :mbchan->IMAPCredential)
                             (* 5 60 1000) ; FIXME: Move to config
                             cmd-chan-1
                             log-chan-0)
        cmd-chan-2 (:cmd-chan-out connection-watcher)]
    (Application.
      cmd-chan-0
      (->LoggingService DEBUG
                        (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
                        log-chan-1)
      notification-service
      (->MbsyncMaster (:mbsyncrc config)
                      cmd-chan-2
                      log-chan-0)
      connection-watcher
      sync-timer)))
