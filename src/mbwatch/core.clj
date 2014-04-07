(ns mbwatch.core
  "
       ──── Command ─────────┐
                             │
                             │
                             ▼                     ──┐
                   ┌───────────────────┐             │
                   │ ConnectionWatcher │             │
                   └─────────┬─────────┘             │
                             │                       │
                             │                       │
                             ▼                       │
                      ┌──────────────┐               │
                      │ MbsyncMaster │               ├─── Loggable ─┐
                      └──────┬───────┘               │              │
                             │                       │              │
                 ┌───────────┴──────────┐            │              │
                 ▼                      ▼            │              │
          ┌──────────────┐       ┌──────────────┐    │              │
          │ MbsyncWorker │   …   │ MbsyncWorker │    │              │
          └──────────────┘       └──────────────┘    │              │
                                                   ──┘              │
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
  (:require [clojure.core.async :refer [chan pipe]]
            [clojure.core.async.impl.protocols :refer [ReadPort]]
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
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (mbwatch.config Config)
           (mbwatch.connection_watcher ConnectionWatcher)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)))

(t/defrecord Application
  [logging-service      :- LoggingService
   notification-service :- NewMessageNotificationService
   mbsync-master        :- MbsyncMaster
   connection-watcher   :- ConnectionWatcher]

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
  (let [;; Create deep pipes for Commands and Loggables
        cmd-chan (pipe cmd-chan (chan CHAN-SIZE))
        log-chan (chan CHAN-SIZE)
        ;; Create middleware components
        notification-service (->NewMessageNotificationService
                               (-> config :mbwatchrc :notify-command)
                               (atom {"self" #{"INBOX"}}) ; FIXME: Move to config
                               log-chan)
        log-chan (:log-chan-in notification-service)
        connection-watcher (->ConnectionWatcher
                             (-> config :mbsyncrc :mbchan->IMAPCredential)
                             (* 60 1000) ; FIXME: Move to config
                             cmd-chan
                             log-chan)
        cmd-chan (:cmd-chan-out connection-watcher)]
    (Application.
      (->LoggingService DEBUG
                        (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
                        (:log-chan-out notification-service))
      notification-service
      (->MbsyncMaster (:mbsyncrc config)
                      cmd-chan
                      log-chan)
      connection-watcher)))
