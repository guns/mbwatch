(ns mbwatch.notification
  "NewMessageNotificationService is a Loggable middleware that tracks
   SyncCommands and spawns a notification when all requested channels have
   been synchronized.

                      ┌───────────────────────────────┐
     ─── Loggable ──▶ │ NewMessageNotificationService │ ─── Loggable ──▶
                      └───────────────────────────────┘
  "
  (:require [clojure.core.async :refer [<!! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.set :as set]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.config :refer [mdir-path]]
            [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.maildir :refer [new-messages senders]]
            [mbwatch.mbsync.command]
            [mbwatch.mbsync.events]
            [mbwatch.process :as process]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [class-name poison-chan thread-loop to-ms
                                  with-chan-value]]
            [schema.core :as s :refer [Int defschema maybe one protocol]]
            [schema.utils :refer [class-schema]])
  (:import (clojure.lang IDeref)
           (javax.mail.internet MimeMessage)
           (mbwatch.logging LogItem)
           (mbwatch.mbsync.command SyncCommand)
           (mbwatch.mbsync.events MbsyncEventStop)
           (org.joda.time DateTime)))

(s/defn ^:private format-msg :- (maybe String)
  [messages :- [MimeMessage]]
  (let [n (count messages)]
    (when (pos? n)
      (format "%d new message%s from:\n\n%s"
              n
              (if (= 1 n) "" \s)
              (string/join \newline (senders messages))))))

(s/defn ^:private sync-event->notification :- (maybe String)
  [notify-map :- {String #{String}}
   event      :- MbsyncEventStop]
  (let [{:keys [mbchan mboxes maildir start]} event]
    (when (and maildir (contains? notify-map mbchan))
      (let [nboxes (notify-map mbchan)
            bs (if (empty? mboxes)
                 nboxes ; [] means full channel sync
                 (set/intersection (set mboxes) nboxes))]
        (when (seq bs)
          (let [ts (to-ms start)]
            (->> (sort bs)
                 (mapv #(format "[%s/%s] %s"
                                mbchan % (format-msg
                                           (new-messages (mdir-path maildir %) ts))))
                 (string/join "\n\n"))))))))

(s/defn ^:private new-message-notification :- (maybe String)
  [notify-map :- {String #{String}}
   events     :- [MbsyncEventStop]]
  (let [msgs (->> events
                  (map (partial sync-event->notification notify-map))
                  (remove nil?))]
    (when (seq msgs)
      (string/join "\n\n" msgs))))

(s/defn notify! :- VOID
  [notify-cmd :- String
   notify-map :- {String #{String}}
   events     :- MbsyncEventStop]
  (when-let [msg (new-message-notification notify-map events)]
    (process/spawn "bash" "-c" notify-cmd :in msg)
    nil))

(declare handle-notification-input)

(s/defrecord NewMessageNotificationService
  [notify-cmd     :- String
   notify-map-ref :- IDeref
   notify-chan    :- ReadPort
   log-chan       :- WritePort
   state-chan     :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (put! log-chan this)
    (assoc this :state-chan
           (thread-loop [sync-requests {}]
             (with-chan-value [obj (<!! notify-chan)]
               ;; Pass through ASAP
               (put! log-chan obj)
               (recur (handle-notification-input sync-requests obj this))))))

  (stop [this]
    (put! notify-chan this)
    (poison-chan notify-chan state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (let [msg (format "%s %s: `%s`"
                      (if state-chan "↓ Stopping" "↑ Starting")
                      (class-name this)
                      notify-cmd)]
      (LogItem. DEBUG (DateTime.) msg))))

(defschema SyncRequestMap
  {Int [(one Int "needed event count") MbsyncEventStop]})

(s/defn ^:private handle-notification-input :- SyncRequestMap
  {:require [SyncCommand MbsyncEventStop]}
  [sync-requests :- SyncRequestMap
   obj           :- Object
   service-map   :- (:schema (class-schema NewMessageNotificationService))]
  (case (class obj)
    #=mbwatch.mbsync.command.SyncCommand
    (let [{:keys [id mbchan->mbox]} obj]
      (assoc sync-requests id [(count mbchan->mbox)]))

    #=mbwatch.mbsync.events.MbsyncEventStop
    (let [{:keys [id mbchan]} obj]
      (if-let [req (sync-requests id)]
        (let [req (conj req obj)]
          (if (> (count req) (first req))
            (do (notify! (:notify-cmd service-map)
                         (deref (:notify-map-ref service-map))
                         (rest req))
                (dissoc sync-requests id))
            (assoc sync-requests id req)))
        sync-requests))

    ;; obj is not relevant to our interests
    sync-requests))
