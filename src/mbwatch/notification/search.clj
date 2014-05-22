(ns mbwatch.notification.search
  "Search for and filter new messages for notification.

                             ┌─────────┐
                             │ Message │
                             └────┬────┘
                                  │
                                  ▼
                        ┌───────────────────┐
                        │ Mbox blacklisted? ├─── yes ──▶ skip
                        └─────────┬─────────┘
                                  │ no
                                  ▼
                             ┌──────────┐
       skip ◀─── :none ──────┤ Strategy ├────── :all ──▶ NOTIFY
                             └────┬─────┘
                                  │ :match
                                  ▼
                        ┌───────────────────┐
                        │ Mbox whitelisted? ├─── yes ──▶ NOTIFY
                        └─────────┬─────────┘
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
            [mbwatch.maildir :refer [get-all-mboxes get-mdir
                                     new-message-files]]
            [mbwatch.mbmap :refer [mbmap-disj]]
            [mbwatch.message :refer [headers message senders subject]]
            [mbwatch.time :refer [dt->ms]]
            [mbwatch.types :refer [LowerCaseWord VOID]]
            [schema.core :as s :refer [maybe]])
  (:import (javax.mail.internet MimeMessage)
           (mbwatch.events MbsyncEventStop NewMessageEvent
                           NewMessageSearchEvent)
           (mbwatch.types NewMessageEventData NotifySpec PatternWrapper)
           (org.joda.time DateTime)))

(s/defn ^:private matches? :- Boolean
  [patterns :- {LowerCaseWord #{PatternWrapper}}
   message  :- MimeMessage]
  (if (seq patterns)
    (every? (fn [[name pws]]
              (let [ps (map #(.pattern ^PatternWrapper %) pws)]
                (case name
                  "subject" (let [s (subject message)]
                              (every? #(re-find % s) ps))
                  "from" (let [ss (senders message)]
                           (every? (fn [s] (every? #(re-find % s) ps)) ss))
                  (let [hs (headers message name)]
                    (every? (fn [h] (every? #(re-find % h) ps)) hs)))))
            patterns)
    false))

(s/defn ^:private add-new-message :- NewMessageEventData
  [data :- NewMessageEventData
   msg  :- MimeMessage]
  (NewMessageEventData.
    (inc (.count data))
    (into (.senders data) (senders msg))
    (into (.message-ids data) (headers msg "message-id"))))

(s/defn ^:private new-message-data-by-mbox :- {String NewMessageEventData}
  "Does not check if notifications are disabled!"
  [event       :- MbsyncEventStop
   notify-spec :- NotifySpec]
  (let [{:keys [mbchan mboxes maildir start]} event
        {:keys [strategy blacklist whitelist patterns]} notify-spec
        timestamp (dt->ms start)
        ;; Expand mboxes now so we can accurately disjoin
        mboxes (if (seq mboxes)
                 mboxes
                 (get-all-mboxes maildir))
        ;; Remove blacklisted mboxes
        {mboxes mbchan} (mbmap-disj {mbchan mboxes} blacklist)
        ;; We don't want to analyze messages in whitelisted mboxes or if
        ;; strategy is :all
        {domatch mbchan} (cond
                           (= strategy :all) nil
                           (some? mboxes) (mbmap-disj {mbchan mboxes} whitelist))]
    (reduce
      (fn [m mbox]
        (let [fs (new-message-files (get-mdir maildir mbox) timestamp)
              data (reduce
                     (fn [d f]
                       (let [m (message f)]
                         (if (contains? domatch mbox)
                           (if (matches? patterns m)
                             (add-new-message d m)
                             d)
                           (add-new-message d m))))
                     (NewMessageEventData. 0 #{} #{}) fs)]
          (cond-> m
            (pos? (:count data)) (assoc mbox data))))
      {} mboxes)))

(s/defn ^:private find-new-messages :- (maybe NewMessageEvent)
  [events      :- [MbsyncEventStop]
   notify-spec :- NotifySpec
   log-chan    :- WritePort]
  (when-not (= :none (:strategy notify-spec))
    (let [ev (NewMessageSearchEvent. events (DateTime.) nil)
          _ (put! log-chan ev)
          ;; While we are most likely reading from the filesystem, most of
          ;; the time new messages will be in the disk cache, so there will
          ;; not be any IO wait. Parallelizing the following would shorten
          ;; execution time, but may also have an annoying impact on other
          ;; user programs. Therefore, until it can can be shown that this is
          ;; too slow, we will conduct the search serially.
          m (reduce
              (fn [m ev]
                (let [mbox->data (new-message-data-by-mbox ev notify-spec)]
                  (cond-> m
                    (seq mbox->data) (assoc (:mbchan ev) mbox->data))))
              {} events)]
      (put! log-chan (assoc ev :stop (DateTime.)))
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
  (when-some [new-msg-event (find-new-messages events notify-spec log-chan)]
    (put! log-chan new-msg-event)
    (when (seq notify-cmd)
      (notify! notify-cmd new-msg-event))))
