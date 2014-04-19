(ns mbwatch.core
  "
              ────── Command ─────────┐
                                      │
                                      │ 0
                                      ▼                     ──┐
                                ┌───────────┐                 │
                                │ SyncTimer │                 │
                                └─────┬─────┘                 │
                                      │                       │
                                      │ 1                     │
                                      ▼                       │
                                ┌────────────┐                │
             ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄▷ │ IDLEMaster │                │
             ┊                  └─────┬──────┘                │
             ┊                        │                       │
             ┊             ┌──────────┴──────────┐            │
             ┊             ▼                     ▼            │
             ┊      ┌────────────┐         ┌────────────┐     │
             ┊      │ IDLEWorker │    …    │ IDLEWorker │     │
             ┊      └──────┬─────┘         └─────┬──────┘     │
             ┊             └──────────┬──────────┘            │
             ┊                        │ 2                     ├── Loggable ──┐
             ┊                        ▼                       │              │
   ┌┄┄┄┄┄┄┄┄┄┴┄┄┄┄┄┄┄┄┄┐    ┌───────────────────┐             │              │
   ┊ ConnectionMapAtom ┊ ◁┄┄┤ ConnectionWatcher │             │              │
   └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘    └─────────┬─────────┘             │              │
                                      │                       │              │
                                      │ 3                     │              │
                                      ▼                       │              │
                               ┌──────────────┐               │              │
                               │ MbsyncMaster │               │              │
                               └──────┬───────┘               │              │
                                      │                       │              │
                           ┌──────────┴──────────┐            │              │
                           ▼                     ▼            │              │
                   ┌──────────────┐       ┌──────────────┐    │              │
                   │ MbsyncWorker │   …   │ MbsyncWorker │    │              │
                   └──────────────┘       └──────────────┘    │              │
                                                            ──┘              │
                                                                             │
                                                                             │
                      ┌───────────────────────────────┐                      │
                      │ NewMessageNotificationService │ ◀────────────────────┘
                      └───────────────┬───────────────┘            0
                                      │
                                      │ 1
                                      ▼
                              ┌────────────────┐
                              │ LoggingService │
                              └────────────────┘
  "
  (:require [clojure.core.async :as async :refer [chan close!]]
            [clojure.core.async.impl.protocols :refer [WritePort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.concurrent :refer [CHAN-SIZE]]
            [mbwatch.config]
            [mbwatch.connection-watcher :refer [->ConnectionWatcher]]
            [mbwatch.console-logger :refer [->ConsoleLogger
                                            MILLIS-TIMESTAMP-FORMAT
                                            get-default-colors]]
            [mbwatch.imap :refer [->IDLEMaster]]
            [mbwatch.logging :refer [->LoggingService DEBUG]]
            [mbwatch.mbsync :refer [->MbsyncMaster]]
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.sync-timer :refer [->SyncTimer]]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (mbwatch.config Config)
           (mbwatch.connection_watcher ConnectionWatcher)
           (mbwatch.imap IDLEMaster)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)
           (mbwatch.sync_timer SyncTimer)))

(t/defrecord ^:private Application
  [cmd-chan             :- WritePort
   logging-service      :- LoggingService
   notification-service :- NewMessageNotificationService
   mbsync-master        :- MbsyncMaster
   connection-watcher   :- ConnectionWatcher
   idle-master          :- IDLEMaster
   sync-timer           :- SyncTimer]

  Lifecycle

  ;; While the application components do not explicitly depend on each other,
  ;; we do generally want the LogItem consumers to start before the producers
  ;; and stop after them. Here we depend on the implicit parameter-ordering of
  ;; (keys a-record) to start and stop the components in FILO order.

  (start [this]
    (start-system this (rest (keys this))))

  (stop [this]
    (close! cmd-chan)
    (stop-system this (rest (keys this)))))

(s/defn ->Application :- Application
  [config :- Config]
  (let [connections-atom (atom {})
        ;; Command pipeline
        sync-timer (->SyncTimer {} ; FIXME: Move to config
                                (chan CHAN-SIZE)
                                (-> config :mbwatchrc :sync-timer-period))
        cmd-chan-0 (:cmd-chan-in sync-timer)
        cmd-chan-1 (:cmd-chan-out sync-timer)
        ;; ->
        idle-master (->IDLEMaster (-> config :mbsyncrc :mbchan->IMAPCredential)
                                  {"self" #{"INBOX"}} ; FIXME: Move to config
                                  connections-atom
                                  (-> config :mbwatchrc :imap-socket-timeout)
                                  cmd-chan-1)
        cmd-chan-2 (:cmd-chan-out idle-master)
        ;; ->
        connection-watcher (->ConnectionWatcher
                             connections-atom
                             (-> config :mbsyncrc :mbchan->IMAPCredential)
                             (-> config :mbwatchrc :connection-period)
                             (-> config :mbwatchrc :connection-timeout)
                             cmd-chan-1)
        cmd-chan-3 (:cmd-chan-out connection-watcher)
        ;; ->
        mbsync-master (->MbsyncMaster (:mbsyncrc config)
                                      cmd-chan-3)
        ;; Logging pipeline
        log-chan-0 (-> (mapv :log-chan [sync-timer
                                        idle-master
                                        connection-watcher
                                        mbsync-master])
                       (async/merge CHAN-SIZE))
        notification-service (->NewMessageNotificationService
                               (-> config :mbwatchrc :notify-command)
                               {"self" #{"INBOX"}} ; FIXME: Move to config
                               log-chan-0)
        log-chan-1 (:log-chan-out notification-service)
        ;; ->
        logging-service (->LoggingService
                          DEBUG
                          (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
                          log-chan-1)]
    (Application. cmd-chan-0
                  logging-service
                  notification-service
                  mbsync-master
                  connection-watcher
                  idle-master
                  sync-timer)))
