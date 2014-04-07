(ns mbwatch.notification
  "NewMessageNotificationService is a Loggable middleware that tracks :sync
   Commands and spawns a notification when all requested mbchans have been
   synchronized.

                      ┌───────────────────────────────┐
     ─── Loggable ──▶ │ NewMessageNotificationService ├──── Loggable ──▶
                      └───────────────────────────────┘
  "
  (:require [clojure.core.async :refer [<!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.set :refer [intersection]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        thread-loop]]
            [mbwatch.config :refer [mdir-path]]
            [mbwatch.logging :refer [->LogItem DEBUG INFO Loggable log!]]
            [mbwatch.maildir :refer [new-messages senders]]
            [mbwatch.mbsync.events]
            [mbwatch.process :as process]
            [mbwatch.types :as t :refer [VOID]]
            [mbwatch.util :refer [to-ms]]
            [schema.core :as s :refer [Int defschema maybe]])
  (:import (clojure.lang IDeref)
           (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (javax.mail.internet MimeMessage)
           (mbwatch.command Command)
           (mbwatch.mbsync.events MbsyncEventStop MbsyncUnknownChannelError)
           (org.joda.time DateTime)))

(def ^:private ^:const MAX-SENDERS-SHOWN
  "TODO: Make configurable?"
  8)

(t/defrecord NewMessageNotification
  [mbchan->mbox->messages :- {String {String [MimeMessage]}}
   timestamp              :- DateTime]

  Loggable

  (log-level [_] INFO)

  (log-item [this]
    (let [sb (reduce
               (fn [s [mbchan mbox->messages]]
                 (reduce
                   (fn [^StringBuilder s [mbox messages]]
                     (.append s (format " [%s/%s %d]" mbchan mbox (count messages))))
                   s (sort mbox->messages)))
               (StringBuilder. "NewMessageNotification:") (sort mbchan->mbox->messages))]
      (->LogItem this (str sb)))))

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
                 nboxes ; [] means full sync
                 (intersection (set mboxes) nboxes))
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
  [notify-command :- String
   notification   :- NewMessageNotification]
  (let [msgs (mapcat
               (fn [[mbchan mbox->messages]]
                 (map
                   (fn [[mbox messages]]
                     (format "[%s/%s]\t%s" mbchan mbox (format-msg messages)))
                   (sort mbox->messages)))
               (sort (:mbchan->mbox->messages notification)))
        proc (process/spawn
               "bash" "-c" notify-command :in (string/join "\n\n" msgs))]
    (when-not (zero? (.waitFor proc))
      (let [ebuf (StringWriter.)
            _ (process/dump! proc :err ebuf)
            emsg (str ebuf)
            emsg (format "`%s` failed with status %d.%s"
                         notify-command
                         (.exitValue proc)
                         (if (seq emsg) (str "\n" emsg) ""))]
        (throw (RuntimeException. emsg))))))

(defschema ^:private SyncRequestMap
  {Int {:countdown Int
        :events [MbsyncEventStop]}})

(defprotocol ^:private INotification
  (^:private
    process-event [this sync-requests notify-service]
    "Returns a new version of the sync-requests map, adding or removing self
     from it as necessary."))

(t/defrecord NewMessageNotificationService
  [notify-command :- String
   notify-map-ref :- IDeref
   log-chan-in    :- ReadPort
   log-chan-out   :- WritePort
   status         :- AtomicBoolean
   exit-fn        :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log! log-chan-out this)
    (let [c (thread-loop [sync-requests {}]
              (when (.get status)
                (when-some [obj (<!! log-chan-in)]
                  ;; Pass through ASAP
                  (put! log-chan-out obj)
                  (recur (process-event obj sync-requests this)))))]
      (assoc this :exit-fn #(<!! c))))

  (stop [this]
    ;; Exit ASAP
    (.set status false)
    (log! log-chan-out this)
    (close! log-chan-in)
    (exit-fn)
    (dissoc this :exit-fn))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-fn
                      "↓ Stopping NewMessageNotificationService"
                      "↑ Starting NewMessageNotificationService"))))

(s/defn ->NewMessageNotificationService :- NewMessageNotificationService
  [notify-command :- String
   notify-map-ref :- IDeref
   log-chan-in    :- ReadPort]
  (strict-map->NewMessageNotificationService
    {:notify-command notify-command
     :notify-map-ref notify-map-ref
     :log-chan-in log-chan-in
     :log-chan-out (chan CHAN-SIZE)
     :status (AtomicBoolean. true)
     :exit-fn nil}))

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
          (do (future-catch-print
                (when-let [note (->NewMessageNotification
                                  (deref (:notify-map-ref notify-service))
                                  events)]
                  (put! (:log-chan-out notify-service) note)
                  (notify! (:notify-command notify-service) note)))
              (dissoc sync-requests id))
          (assoc sync-requests id {:countdown countdown :events events})))
      sync-requests)))

(extend-protocol INotification

  Command

  (process-event [this sync-requests _]
    (if (= (:opcode this) :sync)
      (let [{:keys [id payload]} this]
        (assoc sync-requests id {:countdown (count payload) :events []}))
      sync-requests))

  MbsyncEventStop

  (process-event [this sync-requests notify-service]
    (process-stop-event this sync-requests notify-service true))

  MbsyncUnknownChannelError

  (process-event [this sync-requests notify-service]
    (process-stop-event this sync-requests notify-service false))

  Object

  (process-event [_ sync-requests _] sync-requests))
