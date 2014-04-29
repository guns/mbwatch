(ns mbwatch.notification
  "NewMessageNotificationService is a Loggable middleware that tracks :sync
   Commands and spawns a notification when all requested mbchans have been
   synchronized.

   The set of mboxes to notify on can be changed via :notify-* Commands.

                      ┌───────────────────────────────┐
     ─── Loggable ──▶ │ NewMessageNotificationService ├──── Loggable ──▶
                      └───────────────────────────────┘
  "
  (:require [clojure.core.async :refer [<!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.set :refer [intersection]]
            [clojure.string :as string]
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [CommandSchema]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        thread-loop]]
            [mbwatch.events :refer [->NewMessageNotification
                                    ->UserCommandFeedback]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.maildir :refer [mdir-path new-messages senders]]
            [mbwatch.mbmap :refer [mbmap-diff mbmap-disj mbmap-merge
                                   mbtuples->mbmap]]
            [mbwatch.process :as process]
            [mbwatch.time :refer [dt->ms]]
            [mbwatch.types :as t :refer [MBMap MBMapAtom VOID]]
            [mbwatch.util :refer [case+ when-seq]]
            [schema.core :as s :refer [Int defschema either maybe]])
  (:import (clojure.lang IFn)
           (java.io StringWriter)
           (java.util.concurrent.atomic AtomicBoolean)
           (javax.mail.internet MimeMessage)
           (mbwatch.command Command)
           (mbwatch.events MbsyncEventStop MbsyncUnknownChannelError
                           NewMessageNotification)))

(def ^:private ^:const MAX-SENDERS-SHOWN
  "TODO: Make configurable?"
  8)

(s/defn ^:private format-msg :- (maybe String)
  [messages :- [MimeMessage]]
  (let [msg-count (count messages)
        ss (senders messages)
        n+ (- (count ss) MAX-SENDERS-SHOWN)
        ss (if (pos? n+)
             (conj (subvec ss 0 MAX-SENDERS-SHOWN)
                   (format "… and %d other%s"
                           n+
                           (if (= n+ 1) "" \s)))
             ss)]
    (when (pos? msg-count)
      (format "%d new message%s from:\n%s"
              msg-count
              (if (= msg-count 1) "" \s)
              (string/join \newline ss)))))

(s/defn ^:private sync-event->new-messages-by-box :- (maybe {String [MimeMessage]})
  [notify-map :- MBMap
   event      :- MbsyncEventStop]
  (let [{:keys [mbchan mboxes maildir start]} event]
    (when (and maildir (contains? notify-map mbchan))
      (let [nboxes (notify-map mbchan)
            bs (if (empty? mboxes)
                 nboxes ; #{} means full sync
                 (intersection mboxes nboxes))
            timestamp (dt->ms start)]
        (reduce
          (fn [m b]
            (let [msgs (new-messages (mdir-path maildir b) timestamp)]
              (cond-> m
                (seq msgs) (assoc b msgs))))
          {} bs)))))

(s/defn ^:private find-new-messages :- (maybe NewMessageNotification)
  [notify-map :- MBMap
   events     :- [MbsyncEventStop]]
  (let [m (reduce
            (fn [m ev]
              (let [bs->msgs (sync-event->new-messages-by-box notify-map ev)]
                (cond-> m
                  (seq bs->msgs) (assoc (:mbchan ev) bs->msgs))))
            {} events)]
    (when (seq m)
      (->NewMessageNotification m))))

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
    process-event [this sync-req-map notify-service]
    "Returns a new version of sync-req-map, adding or removing self from it as
     necessary."))

(t/defrecord NewMessageNotificationService
  [notify-command  :- String
   notify-map-atom :- MBMapAtom
   log-chan-in     :- ReadPort
   log-chan-out    :- WritePort
   status          :- AtomicBoolean
   exit-fn         :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan-out this)
    (let [c (thread-loop [sync-req-map {}]
              (when (.get status)
                (when-some [obj (<!! log-chan-in)]
                  ;; Pass through ASAP
                  (put! log-chan-out obj)
                  (recur (process-event obj sync-req-map this)))))]
      (assoc this :exit-fn
             #(do (.set status false)       ; Stop after current iteration
                  (<!! c)
                  (close! log-chan-out))))) ; CLOSE log-chan-out

  (stop [this]
    (log-with-timestamp! log-chan-out this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (if exit-fn
                      "↓ Stopping NewMessageNotificationService"
                      "↑ Starting NewMessageNotificationService"))))

(s/defn ->NewMessageNotificationService :- NewMessageNotificationService
  [notify-command :- String
   notify-map     :- MBMap
   log-chan-in    :- ReadPort]
  (strict-map->NewMessageNotificationService
    {:notify-command notify-command
     :notify-map-atom (atom notify-map)
     :log-chan-in log-chan-in
     :log-chan-out (chan CHAN-SIZE) ; OPEN log-chan-out
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private alter-notify-map-atom! :- VOID
  [alter-fn       :- IFn
   notify-service :- NewMessageNotificationService
   merge-fn       :- (maybe IFn)
   command        :- CommandSchema]
  (let [{:keys [notify-map-atom log-chan-out]} notify-service
        {:keys [payload]} command
        old-map @notify-map-atom
        new-map (if merge-fn
                  (alter-fn notify-map-atom merge-fn payload)
                  (alter-fn notify-map-atom payload))]
    (when-not (= old-map new-map)
      (put! log-chan-out (->UserCommandFeedback :notify/Δ new-map)))
    nil))

(s/defn ^:private process-command :- SyncRequestMap
  [command        :- CommandSchema
   sync-req-map   :- SyncRequestMap
   notify-service :- NewMessageNotificationService]
  (case+ (:opcode command)
    :sync (let [{:keys [id payload]} command]
            (assoc sync-req-map id {:countdown (count payload) :events []}))
    :idle/set (let [[_ Δ+] (mbmap-diff @(:notify-map-atom notify-service) (:payload command))
                    m (mbtuples->mbmap Δ+)]
                (alter-notify-map-atom!
                  swap! notify-service mbmap-merge (assoc command :payload m))
                sync-req-map)
    [:idle/add
     :notify/add] (do (alter-notify-map-atom!
                        swap! notify-service mbmap-merge command)
                      sync-req-map)
    :notify/remove (do (alter-notify-map-atom!
                         swap! notify-service mbmap-disj command)
                       sync-req-map)
    :notify/set (do (alter-notify-map-atom!
                      reset! notify-service nil command)
                    sync-req-map)
    sync-req-map))

(s/defn ^:private process-stop-event :- SyncRequestMap
  [event          :- (either MbsyncEventStop MbsyncUnknownChannelError)
   sync-req-map   :- SyncRequestMap
   notify-service :- NewMessageNotificationService
   conj-event?    :- Boolean]
  (let [{:keys [id mbchan]} event]
    (if-some [req (sync-req-map id)]
      (let [{:keys [countdown events]} req
            countdown (dec countdown)
            events (cond-> events
                     conj-event? (conj event))]
        (if (zero? countdown)
          (do (future-catch-print
                (when-some [note (find-new-messages
                                   (deref (:notify-map-atom notify-service))
                                   events)]
                  (put! (:log-chan-out notify-service) note)
                  (when-seq [cmd (:notify-command notify-service)]
                    (notify! cmd note))))
              (dissoc sync-req-map id))
          (assoc sync-req-map id {:countdown countdown :events events})))
      sync-req-map)))

(extend-protocol INotification

  Command

  (process-event [this sync-req-map notify-service]
    (process-command this sync-req-map notify-service))

  MbsyncEventStop

  (process-event [this sync-req-map notify-service]
    (process-stop-event this sync-req-map notify-service true))

  MbsyncUnknownChannelError

  (process-event [this sync-req-map notify-service]
    (process-stop-event this sync-req-map notify-service false))

  Object

  (process-event [_ sync-req-map _] sync-req-map))
