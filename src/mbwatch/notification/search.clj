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
            [mbwatch.maildir :refer [get-all-mboxes get-mdir new-messages]]
            [mbwatch.mbmap :refer [mbmap-disj]]
            [mbwatch.message :refer [headers senders subject]]
            [mbwatch.time :refer [dt->ms]]
            [mbwatch.types :refer [LowerCaseWord VOID]]
            [schema.core :as s :refer [maybe]])
  (:import (java.util.regex Pattern)
           (javax.mail.internet MimeMessage)
           (mbwatch.events MbsyncEventStop NewMessageEvent
                           NewMessageSearchEvent)
           (mbwatch.types NotifySpec PatternWrapper)
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

(s/defn ^:private new-messages-by-box :- {String [MimeMessage]}
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
        {mboxes* mbchan} (cond
                           (= strategy :all) nil
                           (some? mboxes) (mbmap-disj {mbchan mboxes} whitelist))]
    (reduce
      (fn [m mbox]
        (let [msgs (cond->> (new-messages (get-mdir maildir mbox) timestamp)
                     (contains? mboxes* mbox) (filterv (partial matches? patterns)))]
          (cond-> m
            (seq msgs) (assoc mbox msgs))))
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
                (let [bs->msgs (new-messages-by-box ev notify-spec)]
                  (cond-> m
                    (seq bs->msgs) (assoc (:mbchan ev) bs->msgs))))
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
