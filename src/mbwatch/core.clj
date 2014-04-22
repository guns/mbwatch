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
  (:require [clojure.core.async :as async :refer [>!! chan close!]]
            [clojure.core.async.impl.protocols :refer [WritePort]]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.cli :refer [parse-argv!]]
            [mbwatch.command :refer [->Command]]
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
           (mbwatch.sync_timer SyncTimer))
  (:gen-class))

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
        sync-timer (->SyncTimer (-> config :sync)
                                (chan CHAN-SIZE)
                                (-> config :sync-period))
        cmd-chan-0 (:cmd-chan-in sync-timer)
        cmd-chan-1 (:cmd-chan-out sync-timer)
        ;; ->
        idle-master (->IDLEMaster (-> config :mbsyncrc :mbchan->IMAPCredential)
                                  (-> config :idle)
                                  connections-atom
                                  (-> config :imap-timeout)
                                  cmd-chan-1)
        cmd-chan-2 (:cmd-chan-out idle-master)
        ;; ->
        connection-watcher (->ConnectionWatcher
                             connections-atom
                             (-> config :mbsyncrc :mbchan->IMAPCredential)
                             (-> config :conn-period)
                             (-> config :conn-timeout)
                             cmd-chan-2)
        cmd-chan-3 (:cmd-chan-out connection-watcher)
        ;; ->
        mbsync-master (->MbsyncMaster (-> config :mbsyncrc)
                                      cmd-chan-3)
        ;; Logging pipeline
        log-chan-0 (-> (mapv :log-chan [sync-timer
                                        idle-master
                                        connection-watcher
                                        mbsync-master])
                       (async/merge CHAN-SIZE))
        notification-service (->NewMessageNotificationService
                               (-> config :notify-cmd)
                               (-> config :notify)
                               log-chan-0)
        log-chan-1 (:log-chan-out notification-service)
        ;; ->
        logging-service (->LoggingService
                          DEBUG
                          (->ConsoleLogger System/out (get-default-colors) MILLIS-TIMESTAMP-FORMAT)
                          log-chan-1)]
    ;; Initial sync
    (>!! cmd-chan-0 (->Command :sync (-> config :sync)))
    (Application. cmd-chan-0
                  logging-service
                  notification-service
                  mbsync-master
                  connection-watcher
                  idle-master
                  sync-timer)))

(defn -main
  "Command line entry point."
  [& argv]
  (try
    (parse-argv! argv)
    (finally
      (shutdown-agents))))
