(ns mbwatch.notification.search
  "Search for and filter new messages for notification.

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
       skip ◀─── :none ──────┤ Strategy ├────── :all ──▶ NOTIFY
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
  (:require [clojure.core.async :refer [put!]]
            [clojure.core.async.impl.protocols :refer [WritePort]]
            [mbwatch.events :refer [->NewMessageEvent]]
            [mbwatch.mbmap :refer [mbmap-subset?]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s :refer [maybe]])
  (:import (javax.mail.internet MimeMessage)
           (mbwatch.events MbsyncEventStop NewMessageEvent)
           (mbwatch.types NotifySpec)))

(s/defn ^:private new-messages-by-box :- (maybe {String [MimeMessage]})
  "Assumes NotifySpec strategy is either :all or :match."
  [event       :- MbsyncEventStop
   notify-spec :- NotifySpec]
  (let [{:keys [mbchan mboxes maildir start]} event
        {:keys [strategy blacklist whitelist references patterns]} notify-spec]
    (cond
      ;; Blacklisted?
      (mbmap-subset? blacklist {mbchan mboxes}) nil
      )))

(s/defn ^:private find-new-messages :- (maybe NewMessageEvent)
  [events      :- [MbsyncEventStop]
   notify-spec :- NotifySpec]
  (when-not (= :none (:strategy notify-spec))
    (let [m (reduce
              (fn [m ev]
                (let [bs->msgs (new-messages-by-box ev notify-spec)]
                  (cond-> m
                    (seq bs->msgs) (assoc (:mbchan ev) bs->msgs))))
              {} events)]
      (when (seq m)
        (->NewMessageEvent m)))))

(s/defn ^:private notify! :- VOID
  [notify-cmd    :- String
   new-msg-event :- NewMessageEvent]
  )

(s/defn search-and-notify! :- VOID
  [events      :- [MbsyncEventStop]
   notify-spec :- NotifySpec
   notify-cmd  :- String
   log-chan    :- WritePort]
  (when-some [new-msg-event (find-new-messages events notify-spec)]
    (put! log-chan new-msg-event)
    (when (seq notify-cmd)
      (notify! notify-cmd new-msg-event))))
