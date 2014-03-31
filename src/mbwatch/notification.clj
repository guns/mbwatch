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
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable log!]]
            [mbwatch.maildir :refer [new-messages senders]]
            [mbwatch.mbsync.command]
            [mbwatch.mbsync.events]
            [mbwatch.process :as process]
            [mbwatch.types :as t :refer [VOID]]
            [mbwatch.util :refer [poison-chan thread-loop to-ms
                                  with-chan-value]]
            [schema.core :as s :refer [Int defschema maybe protocol]])
  (:import (clojure.lang IDeref)
           (java.io StringWriter)
           (javax.mail.internet MimeMessage)
           (mbwatch.mbsync.command SyncCommand)
           (mbwatch.mbsync.events MbsyncEventStop MbsyncUnknownChannelError)
           (org.joda.time DateTime)))

(def ^:const ^:private MAX-SENDERS-SHOWN
  "TODO: Make configurable?"
  8)

(t/defrecord ^:private NewMessageNotification
  [mbchan->mbox->messages :- {String {String [MimeMessage]}}
   timestamp              :- DateTime]

  Loggable

  (log-level [_] INFO)

  (->log [this]
    (let [ss (mapv (fn [[mbchan mbox->messages]]
                     (let [n (apply + (mapv (comp count val) mbox->messages))]
                       (str mbchan \( n \))))
                   mbchan->mbox->messages)]
      (->LogItem this (str "New messages: " (string/join " " ss))))))

(s/defn ^:private format-msg :- (maybe String)
  [messages :- [MimeMessage]]
  (let [n (count messages)
        ss (vec (senders messages))
        ss (if (> (count ss) MAX-SENDERS-SHOWN)
             (conj (subvec ss 0 MAX-SENDERS-SHOWN)
                   (format "… and %d other%s"
                           (- (count ss) MAX-SENDERS-SHOWN)
                           (if (= n 1) "" \s)))
             ss)]
    (when (pos? n)
      (format "%d new message%s from:\n%s"
              n
              (if (= n 1) "" \s)
              (string/join \newline ss)))))

(s/defn ^:private sync-event->new-messages-by-box :- (maybe {String [MimeMessage]})
  [notify-map :- {String #{String}}
   event      :- MbsyncEventStop]
  (let [{:keys [mbchan mboxes maildir start]} event]
    (when (and maildir (contains? notify-map mbchan))
      (let [nboxes (notify-map mbchan)
            bs (if (empty? mboxes)
                 nboxes ; [] means full channel sync
                 (set/intersection (set mboxes) nboxes))
            timestamp (to-ms start)]
        (reduce
          (fn [m b]
            (let [msgs (new-messages (mdir-path maildir b) timestamp)]
              (cond-> m
                (seq msgs) (assoc b msgs))))
          {} (sort bs))))))

(s/defn ^:private ->NewMessageNotification :- (maybe NewMessageNotification)
  [notify-map :- {String #{String}}
   events     :- [MbsyncEventStop]]
  (let [m (reduce
            (fn [m ev]
              (let [bs->msgs (sync-event->new-messages-by-box notify-map ev)]
                (cond-> m
                  (seq bs->msgs) (assoc (:mbchan ev) bs->msgs))))
            {} events)]
    (when (seq m)
      (NewMessageNotification. m (DateTime.)))))

(s/defn ^:private notify! :- VOID
  [notify-cmd   :- String
   notification :- NewMessageNotification]
  (let [msgs (mapcat
               (fn [[mbchan mbox->messages]]
                 (map
                   (fn [[mbox messages]]
                     (format "[%s/%s]\t%s" mbchan mbox (format-msg messages)))
                   (sort mbox->messages)))
               (sort (:mbchan->mbox->messages notification)))
        proc (process/spawn
               "bash" "-c" notify-cmd :in (string/join "\n\n" msgs))]
    (when-not (zero? (.waitFor proc))
      (let [ebuf (StringWriter.)
            _ (process/dump! proc :err ebuf)
            emsg (str ebuf)
            emsg (format "`%s` failed with status %d.%s"
                         notify-cmd
                         (.exitValue proc)
                         (if (seq emsg) (str "\n" emsg) ""))]
        (throw (RuntimeException. emsg))))))

(defschema SyncRequestMap
  {Int {:countdown Int
        :events [MbsyncEventStop]}})

(defprotocol INotification
  (process-event [this sync-requests notify-service]
                 "Returns a new version of the sync-requests map, adding or
                  removing self from it as necessary."))

(t/defrecord NewMessageNotificationService
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
               (recur (process-event obj sync-requests this))))))

  (stop [this]
    (log! read-chan this)
    (poison-chan read-chan state-chan)
    (dissoc this :state-chan))

  Loggable

  (log-level [_] DEBUG)

  (->log [this]
    (->LogItem this (format "%s NewMessageNotificationService: `%s`"
                            (if state-chan "↓ Stopping" "↑ Starting")
                            notify-cmd))))

(s/defn ^:private process-stop-event :- SyncRequestMap
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
                (try
                  (when-let [note (->NewMessageNotification
                                    (deref (:notify-map-ref notify-service))
                                    events)]
                    (put! (:write-chan notify-service) note)
                    (notify! (:notify-cmd notify-service) note))
                  (catch Throwable e
                    (.println System/err e))))
              (dissoc sync-requests id))
          (assoc sync-requests id {:countdown countdown :events events})))
      sync-requests)))

(extend-protocol INotification

  SyncCommand

  (process-event [this sync-requests notify-service]
    (let [{:keys [id mbchan->mbox]} this]
      (assoc sync-requests id {:countdown (count mbchan->mbox) :events []})))

  MbsyncEventStop

  (process-event [this sync-requests notify-service]
    (process-stop-event this sync-requests notify-service true))

  MbsyncUnknownChannelError

  (process-event [this sync-requests notify-service]
    (process-stop-event this sync-requests notify-service false))

  Object

  (process-event [_ sync-requests _] sync-requests))
