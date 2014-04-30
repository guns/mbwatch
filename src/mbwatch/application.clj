(ns mbwatch.application
  "
   ──────── Loggable ────────────────────────────────────────────────────────┐
                                                                             │
   ──────── Command ──────────────────┐                                      │
                                      │                                      │
                                      │ 0                                    │
                                      ▼                     ──┐              │
                                ┌───────────┐                 │              │
                                │ SyncTimer │                 │              │
                                └─────┬─────┘                 │              │
                                      │                       │              │
                                      │ 1                     │              │
                                      ▼                       │              │
                                ┌────────────┐                │              │
             ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄▷ │ IDLEMaster │                │              │
             ┊                  └─────┬──────┘                │              │
             ┊                        │                       │              │
             ┊             ┌──────────┴──────────┐            │              │
             ┊             ▼                     ▼            │              │
             ┊      ┌────────────┐         ┌────────────┐     │              │
             ┊      │ IDLEWorker │    …    │ IDLEWorker │     │              │
             ┊      └──────┬─────┘         └─────┬──────┘     │              │
             ┊             └──────────┬──────────┘            │              │
             ┊                        │ 2                     ├── Loggable ──┤
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
  (:require [clojure.core.async :as async :refer [put!]]
            [clojure.core.async.impl.protocols :refer [WritePort]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle start-system
                                                stop-system]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE]]
            [mbwatch.config]
            [mbwatch.connection-watcher :refer [->ConnectionWatcher]]
            [mbwatch.console :refer [->ConsoleLogger MILLIS-TIMESTAMP-FORMAT
                                     TIMESTAMP-FORMAT get-default-colors
                                     tty?]]
            [mbwatch.imap :refer [->IDLEMaster]]
            [mbwatch.logging :refer [->LoggingService DEBUG]]
            [mbwatch.mbmap :refer [join-mbentry join-mbmap]]
            [mbwatch.mbsync :refer [->MbsyncMaster]]
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.sync-timer :refer [->SyncTimer]]
            [mbwatch.time :refer [human-duration]]
            [mbwatch.types :as t :refer [MapAtom]]
            [mbwatch.util :refer [make-table multi-row-entry]]
            [schema.core :as s :refer [maybe]])
  (:import (mbwatch.concurrent Timer)
           (mbwatch.config Config)
           (mbwatch.connection_watcher ConnectionWatcher)
           (mbwatch.imap IDLEMaster)
           (mbwatch.logging LoggingService)
           (mbwatch.mbsync MbsyncMaster)
           (mbwatch.notification NewMessageNotificationService)
           (mbwatch.sync_timer SyncTimer)
           (org.joda.time DateTime)))

(t/defrecord Application
  [cmd-chan             :- WritePort
   log-chan             :- WritePort
   cache-atom           :- (maybe MapAtom)
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
    (start-system this (drop 3 (keys this))))

  (stop [this]
    (stop-system this (drop 3 (keys this)))))

(s/defn ->Application :- Application
  [config   :- Config
   cmd-chan :- WritePort
   log-chan :- WritePort]
  (let [connections-atom (atom {})
        cache-atom (when (:cache-passwords config) (atom {}))
        ;; Command pipeline
        sync-timer (->SyncTimer (:sync config)
                                cmd-chan
                                (:sync-period config))
        cmd-chan-0 (:cmd-chan-in sync-timer)
        cmd-chan-1 (:cmd-chan-out sync-timer)
        ;; ->
        idle-master (->IDLEMaster (-> config :mbsyncrc :mbchan->IMAPCredential)
                                  (:idle config)
                                  cache-atom
                                  connections-atom
                                  (:imap-timeout config)
                                  cmd-chan-1)
        cmd-chan-2 (:cmd-chan-out idle-master)
        ;; ->
        connection-watcher (->ConnectionWatcher
                             connections-atom
                             (-> config :mbsyncrc :mbchan->IMAPCredential)
                             (:conn-period config)
                             (:conn-timeout config)
                             cmd-chan-2)
        cmd-chan-3 (:cmd-chan-out connection-watcher)
        ;; ->
        mbsync-master (->MbsyncMaster (:mbsyncrc config)
                                      cache-atom
                                      cmd-chan-3)
        ;; Logging pipeline
        log-chan-0 (-> (mapv :log-chan [sync-timer
                                        idle-master
                                        connection-watcher
                                        mbsync-master])
                       (conj log-chan)
                       (async/merge CHAN-SIZE))
        notification-service (->NewMessageNotificationService
                               (:notify-cmd config)
                               (:notify config)
                               (:blacklist config)
                               (-> config :mbsyncrc :mbchan->Maildirstore)
                               log-chan-0)
        log-chan-1 (:log-chan-out notification-service)
        ;; ->
        logging-service (->LoggingService
                          (:log-level config)
                          (->ConsoleLogger System/out
                                           (when (tty?) (get-default-colors))
                                           (if (>= (:log-level config) DEBUG)
                                             MILLIS-TIMESTAMP-FORMAT
                                             TIMESTAMP-FORMAT))
                          log-chan-1)]
    ;; Initial sync
    (put! cmd-chan (->Command :sync (:sync config)))
    (Application. cmd-chan
                  log-chan
                  cache-atom
                  logging-service
                  notification-service
                  mbsync-master
                  connection-watcher
                  idle-master
                  sync-timer)))

(s/defn ^:private timer-status :- String
  [timer :- Timer]
  (let [{:keys [period alarm]} timer]
    (format "%-20s %s"
            (if (zero? period)
              "disabled"
              (human-duration period))
            (if (pos? alarm)
              (str "next: " (human-duration (- alarm (System/currentTimeMillis))))
              ""))))

(s/defn status-table :- String
  [application :- Application]
  (let [{:keys [cache-atom logging-service notification-service mbsync-master
                connection-watcher idle-master sync-timer]} application]
    (make-table
      (concat
        [["Current time" (.print TIMESTAMP-FORMAT (DateTime.))]
         ["idle" (join-mbmap @(:idle-map-atom idle-master))]
         ["sync" (join-mbmap @(:sync-req-atom sync-timer))]
         ["notify" (join-mbmap @(:notify-map-atom notification-service))]
         ["blacklist" (join-mbmap @(:blacklist-map-atom notification-service))]
         ["conn-period" (timer-status @(:timer-atom connection-watcher))]
         ["sync-period" (timer-status @(:timer-atom sync-timer))]
         ["cache-passwords" (str (some? cache-atom))]]
        (multi-row-entry
          "Connections"
          (mapv (fn [[mbchan {:keys [status pending-syncs]}]]
                  (format "[%4s] %-13s %s"
                          (if status " OK " "FAIL")
                          mbchan
                          (if (and (not status) pending-syncs)
                            (str "pending syncs: "
                                 (string/join \, pending-syncs))
                            "")))
                @(:connections-atom connection-watcher)))
        (multi-row-entry
          "Processes"
          (mapv (fn [{:keys [mbchan mboxes start]}]
                  (format "`mbsync %s` elapsed: %s"
                          (join-mbentry mbchan mboxes)
                          (human-duration start (DateTime.))))
                @(:events-atom mbsync-master)))))))
