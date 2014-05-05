(ns mbwatch.notification
  "NewMessageNotificationService is a Loggable middleware that tracks :sync
   Commands and spawns a notification when all requested mbchans have been
   synchronized.

                      ┌───────────────────────────────┐
     ─── Loggable ──▶ │ NewMessageNotificationService ├──── Loggable ──▶
                      └───────────────────────────────┘

   Messages are selected for notification by the following algorithm:

                                 ┌─────────┐
                                 │ Message │
                                 └────┬────┘
                                      │
                                      ▼
                              ┌───────────────┐
                              │ In blacklist? ├───── yes ──▶ skip
                              └───────┬───────┘
                                      │ no
                                      ▼
                                 ┌──────────┐
          skip  ◀─── :none ──────┤ Strategy ├────── :all ──▶ NOTIFY
                                 └────┬─────┘
                                      │ :match
                                      ▼
                           ┌─────────────────────┐
                           │ Matches references? ├── yes ──▶ NOTIFY
                           └──────────┬──────────┘
                                      │ no
                                      ▼
                              ┌───────────────┐
                              │ In whitelist? ├───── yes ──▶ NOTIFY
                              └───────┬───────┘
                                      │ no
                                      ▼
                            ┌───────────────────┐
                            │ Matches patterns? ├─── yes ──▶ NOTIFY
                            └─────────┬─────────┘
                                      │
                                      ▼
                                     skip
   "
  (:require [clojure.core.async :refer [<!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [com.stuartsierra.component :refer [Lifecycle]]
            [immutable-int-map :refer [int-map]]
            [mbwatch.command :refer [CommandSchema]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-catch-print
                                        thread-loop]]
            [mbwatch.events]
            [mbwatch.logging :refer [->LogItem log-with-timestamp!]]
            [mbwatch.logging.levels :refer [DEBUG]]
            [mbwatch.logging.protocols :refer [Loggable]]
            [mbwatch.notification.search :refer [search-and-notify!]]
            [mbwatch.types :as t :refer [NotifySpecAtom PosInt Word]]
            [schema.core :as s :refer [both defschema maybe]])
  (:import (clojure.lang IFn)
           (immutable_int_map IRadix)
           (java.util.concurrent.atomic AtomicBoolean)
           (mbwatch.command Command)
           (mbwatch.events MbsyncEventStop MbsyncUnknownChannelError)
           (mbwatch.types Maildirstore NotifySpec)))

(def ^:private ^:const MAX-SENDERS-SHOWN
  "TODO: Make configurable?"
  8)

(defschema ^:private SyncRequestMap
  (both IRadix
        {PosInt {:countdown PosInt
                 :events [MbsyncEventStop]}}))

(defprotocol ^:private INotification
  (^:private
    process-event [this sync-req-map notify-service]
    "Returns a new version of sync-req-map, adding or removing self from it as
     necessary."))

(t/defrecord NewMessageNotificationService
  [notify-cmd           :- String
   notify-spec-atom     :- NotifySpecAtom
   mbchan->Maildirstore :- {Word Maildirstore}
   log-chan-in          :- ReadPort
   log-chan-out         :- WritePort
   status               :- AtomicBoolean
   exit-fn              :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan-out this)
    (let [c (thread-loop [sync-req-map (int-map)]
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
    (->LogItem this (format "%s NewMessageNotificationService"
                            (if exit-fn "↓ Stopping " "↑ Starting ")))))

(s/defn ->NewMessageNotificationService :- NewMessageNotificationService
  [notify-cmd           :- String
   notify-spec          :- NotifySpec
   mbchan->Maildirstore :- {Word Maildirstore}
   log-chan-in          :- ReadPort]
  (strict-map->NewMessageNotificationService
    {:notify-cmd notify-cmd
     :notify-spec-atom (atom notify-spec)
     :mbchan->Maildirstore mbchan->Maildirstore
     :log-chan-in log-chan-in
     :log-chan-out (chan CHAN-SIZE) ; OPEN log-chan-out
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private process-command :- SyncRequestMap
  [command        :- CommandSchema
   sync-req-map   :- SyncRequestMap
   notify-service :- NewMessageNotificationService]
  (if (= :sync (:opcode command))
    (let [{:keys [id payload]} command]
      (assoc sync-req-map id {:countdown (count payload) :events []}))
    ;; No other commands effect the sync-req-map
    (do
      (case (:opcode command)
        ;; :notify/whitelist, etc
        nil)
      sync-req-map)))

(s/defn ^:private process-stop-event :- SyncRequestMap
  [event          :- INotification
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
                (search-and-notify! notify-service events))
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
