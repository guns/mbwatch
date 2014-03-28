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
            [mbwatch.logging :refer [->log-item DEBUG Loggable log!]]
            [mbwatch.maildir :refer [new-messages senders]]
            [mbwatch.mbsync.command]
            [mbwatch.mbsync.events]
            [mbwatch.process :as process]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [class-name poison-chan thread-loop to-ms
                                  with-chan-value]]
            [schema.core :as s :refer [Int defschema maybe protocol]])
  (:import (clojure.lang IDeref)
           (javax.mail.internet MimeMessage)
           (mbwatch.mbsync.command SyncCommand)
           (mbwatch.mbsync.events MbsyncEventStop MbsyncUnknownChannelError)))

(def ^:const MAX-SENDERS-SHOWN
  "TODO: Make configurable?"
  8)

(s/defn ^:private format-msg :- (maybe String)
  [messages :- [MimeMessage]]
  (let [n (count messages)
        ss (vec (senders messages))
        ss (if (> (count ss) MAX-SENDERS-SHOWN)
             (conj ss (format "… and %d other%s"
                              (- (count ss) MAX-SENDERS-SHOWN)
                              (if (= n 1) "" \s)))
             ss)]
    (when (pos? n)
      (format "%d new message%s from:\n%s"
              n
              (if (= n 1) "" \s)
              (string/join \newline ss)))))

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
          (let [ts (to-ms start)
                msgs (reduce
                       (fn [v b]
                         (if-let [msg (format-msg (new-messages (mdir-path maildir b) ts))]
                           (conj v (str "[" mbchan "/" b "]\t" msg))
                           v))
                       [] (sort bs))]
            (when (seq msgs)
              (string/join "\n\n" msgs))))))))

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
   events     :- [MbsyncEventStop]]
  (when-let [msg (new-message-notification notify-map events)]
    (process/spawn "bash" "-c" notify-cmd :in msg)
    nil))

(defschema SyncRequestMap
  {Int {:countdown Int
        :events [MbsyncEventStop]}})

(defprotocol INotification
  (process [this sync-requests notify-service]
           "Returns a new version of the sync-requests map, adding or removing
            self from it as necessary."))

(s/defrecord NewMessageNotificationService
  [notify-cmd     :- String
   notify-map-ref :- IDeref
   read-chan      :- ReadPort
   write-chan     :- WritePort
   state-chan     :- (maybe (protocol ReadPort))]

  Lifecycle

  (start [this]
    (log! write-chan this)
    (assoc this :state-chan
           (thread-loop [sync-requests {}]
             (with-chan-value [obj (<!! read-chan)]
               ;; Pass through ASAP
               (put! write-chan obj)
               (recur (process obj sync-requests this))))))

  (stop [this]
    (log! read-chan this)
    (poison-chan read-chan state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (->log-item this (format "%s %s: `%s`"
                             (if state-chan "↓ Stopping" "↑ Starting")
                             (class-name this)
                             notify-cmd))))

(s/defn record-event :- SyncRequestMap
  [obj            :- Object
   sync-requests  :- SyncRequestMap
   notify-service :- NewMessageNotificationService
   conj-event?    :- Boolean]
  (let [{:keys [id mbchan]} obj]
    (if-let [req (sync-requests id)]
      (let [{:keys [countdown events]} req
            countdown (dec countdown)
            events (cond-> events
                     conj-event? (conj obj))]
        (if (zero? countdown)
          (do (future
                (notify! (:notify-cmd notify-service)
                         (deref (:notify-map-ref notify-service))
                         events))
              (dissoc sync-requests id))
          (assoc sync-requests id {:countdown countdown :events events})))
      sync-requests)))

(extend-protocol INotification
  SyncCommand

  (process [this sync-requests notify-service]
    (let [{:keys [id mbchan->mbox]} this]
      (assoc sync-requests id {:countdown (count mbchan->mbox) :events []})))

  MbsyncEventStop

  (process [this sync-requests notify-service]
    (record-event this sync-requests notify-service true))

  MbsyncUnknownChannelError

  (process [this sync-requests notify-service]
    (record-event this sync-requests notify-service false))

  Object

  (process [_ sync-requests _] sync-requests))
