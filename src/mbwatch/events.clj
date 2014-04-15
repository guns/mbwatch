(ns mbwatch.events
  "Loggable event records. Components implementing the Lifecycle protocol
   provide their own Loggable implementations."
  (:require [mbwatch.concurrent]
            [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.logging :refer [ERR INFO Loggable NOTICE WARNING
                                     defloggable]]
            [mbwatch.types :as t :refer [NotifyMap StringList SyncRequest]]
            [mbwatch.util :refer [human-duration join-mbargs
                                  join-sync-request]]
            [schema.core :refer [Int enum maybe]])
  (:import (javax.mail.internet MimeMessage)
           (mbwatch.concurrent Timer)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private connection-event-map
  {true  [NOTICE  " -> reachable"]
   false [WARNING " >! unreachable"]
   nil   [INFO    " -- unregistered"]})

(t/defrecord ConnectionEvent
  [mbchan    :- String
   status    :- (maybe Boolean)
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (first (connection-event-map status)))

  (log-item [this]
    (let [[level suffix] (connection-event-map status)]
      (LogItem. level timestamp (str "Channel " mbchan suffix)))))

(defloggable PendingSyncsEvent INFO
  [action   :- (enum :pool :release)
   sync-req :- SyncRequest]
  (->> sync-req
       join-sync-request
       (str (if (= action :pool)
              "Delaying syncs: "
              "Releasing pending syncs: "))))

(defloggable TimeJumpEvent WARNING
  [retry        :- Int
   ms-per-retry :- Int]
  (if (pos? retry)
    (format "Connection retry #%d in %s" retry (human-duration (* retry ms-per-retry)))
    "Time jump! Retrying connections up to 3 times in the next 90 seconds."))

(defloggable ConnectionWatcherPreferenceEvent INFO
  [type  :- (enum :period)
   timer :- Timer]
  (let [{:keys [period]} timer]
    (case type
      :period (if (zero? period) ; zero?, not pos?, so we don't mask bugs
                "Connection polling disabled."
                (str "Connection polling period set to " (human-duration period))))))

(t/defrecord MbsyncEventStart
  [id     :- Int
   mbchan :- String
   mboxes :- StringList
   start  :- DateTime]

  Loggable

  (log-level [_] INFO)

  (log-item [_]
    (let [msg (format "Starting `mbsync %s`" (join-mbargs mbchan mboxes))]
      (LogItem. INFO start msg))))

(t/defrecord MbsyncEventStop
  [id      :- Int
   mbchan  :- String
   mboxes  :- StringList
   start   :- DateTime
   stop    :- DateTime
   level   :- Int
   status  :- Int
   error   :- (maybe String)
   maildir :- (maybe Maildirstore)]

  Loggable

  (log-level [_] level)

  (log-item [_]
    (let [mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (let [buf (StringBuilder.)
                      fail (if (<= level ERR)
                             (format "FAILURE: `mbsync %s` aborted in %s with status %d."
                                     mbarg Δt status)
                             (format "FAILURE: `mbsync %s` terminated after %s with status %d."
                                     mbarg Δt status))]
                  (.append buf fail)
                  (when error
                    (.append buf \newline)
                    (.append buf error))
                  (str buf)))]
      (LogItem. level stop msg))))

(defloggable MbsyncUnknownChannelError WARNING
  [id     :- Int
   mbchan :- String]
  (format "Unknown channel: `%s`" mbchan))

(defloggable NewMessageNotification INFO
  [mbchan->mbox->messages :- {String {String [MimeMessage]}}]
  (str
    (reduce
      (fn [s [mbchan mbox->messages]]
        (reduce
          (fn [^StringBuilder s [mbox messages]]
            (.append s (format " [%s/%s %d]" mbchan mbox (count messages))))
          s (sort mbox->messages)))
      (StringBuilder. "NewMessageNotification:") (sort mbchan->mbox->messages))))

(defloggable NotifyMapChangeEvent INFO
  [notify-map :- NotifyMap]
  (let [msg (join-sync-request notify-map)]
    (if (seq msg)
      (str "Now notifying on: " msg)
      "Notifications disabled.")))

(defloggable SyncTimerPreferenceEvent INFO
  [type       :- (enum :period :sync-request)
   timer      :- Timer
   sync-req   :- SyncRequest]
  (let [period (:period timer)]
    (case type
      :period (if (zero? period)
                "Sync timer disabled."
                (str "Sync timer period set to: " (human-duration period)))
      :sync-request (if (seq sync-req)
                      (str "Sync timer request set to: " (join-sync-request sync-req))
                      "Sync timer disabled."))))
