(ns mbwatch.events
  "Loggable event records. Components implementing the Lifecycle protocol
   provide their own Loggable implementations."
  (:require [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.logging :refer [ERR INFO Loggable NOTICE WARNING
                                     defloggable]]
            [mbwatch.mbmap :refer [join-mbentry join-mbmap]]
            [mbwatch.time :refer [human-duration]]
            [mbwatch.types :as t :refer [MBMap]]
            [schema.core :as s :refer [Any Int enum maybe]])
  (:import (javax.mail.internet MimeMessage)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private CONNECTION-EVENT-MAP
  {true  [NOTICE  " -> reachable"]
   false [WARNING " >! unreachable"]
   nil   [INFO    " -- unregistered"]})

(def ^:private IMAP-CONNECTION-EVENT-MAP
  {:start      [INFO    "Connecting "]
   :success    [INFO    "Connected "]
   :badauth    [ERR     "Authentication failed for "]
   :failure    [WARNING "Failed to connect "]
   :stop       [INFO    "Disconnecting "]
   :disconnect [WARNING "Disconnected "]
   :lost       [WARNING "Lost connection to "]})

(def ^:private USER-COMMAND-FEEDBACK-MAP
  {:parse-error [WARNING (fn [msg] msg)]
   :conn/clear  [NOTICE (fn [_] "Cleared watched connections")]
   :app/clear   [NOTICE (fn [_] "Cleared password cache")]
   :sync/Δ      [NOTICE (fn [sync-req]
                          (if (seq sync-req)
                            (str "Sync timer request set to: " (join-mbmap sync-req))
                            "Sync timer disabled."))]
   :notify/Δ    [NOTICE (fn [notify-map]
                          (if (seq notify-map)
                            (str "Now notifying on: " (join-mbmap notify-map))
                            "Notifications disabled."))]
   :blacklist/Δ [NOTICE (fn [blacklist-map]
                          (if (seq blacklist-map)
                            (str "Never notifying on: " (join-mbmap blacklist-map))
                            "Blacklist disabled."))]
   :conn/period [NOTICE (fn [{:keys [period]}]
                          (if (zero? period)
                            "Connection polling disabled."
                            (str "Connection polling period set to " (human-duration period))))]
   :sync/period [NOTICE (fn [{:keys [period]}]
                          (if (zero? period) ; zero?, not pos?, so we don't mask bugs
                            "Sync timer disabled."
                            (str "Sync timer period set to: " (human-duration period))))]})

(t/defrecord ConnectionEvent
  [mbchan    :- String
   status    :- (maybe Boolean)
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (first (CONNECTION-EVENT-MAP status)))

  (log-item [this]
    (let [[level suffix] (CONNECTION-EVENT-MAP status)]
      (LogItem. level timestamp (str "Channel " mbchan suffix)))))

(t/defrecord IMAPConnectionEvent
  [type      :- (apply enum (keys IMAP-CONNECTION-EVENT-MAP))
   imap-url  :- String
   error     :- (maybe String)
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (first (IMAP-CONNECTION-EVENT-MAP type)))

  (log-item [this]
    (let [[level prefix] (IMAP-CONNECTION-EVENT-MAP type)
          msg (str prefix imap-url)]
      (LogItem. level timestamp (if error (str msg "\n" error) msg)))))

(t/defrecord UserCommandFeedback
  [type      :- (apply enum (keys USER-COMMAND-FEEDBACK-MAP))
   data      :- Any
   timestamp :- DateTime]

  Loggable

  (log-level [_]
    (first (USER-COMMAND-FEEDBACK-MAP type)))

  (log-item [this]
    (let [[level f] (USER-COMMAND-FEEDBACK-MAP type)
          msg (f data)]
      (LogItem. level timestamp msg))))

(s/defn ->UserCommandFeedback :- UserCommandFeedback
  ([type]
   (->UserCommandFeedback type nil))
  ([type :- (apply enum (keys USER-COMMAND-FEEDBACK-MAP))
    data :- Any]
   (UserCommandFeedback. type data (DateTime.))))

(t/defrecord MbsyncEventStart
  [id     :- Int
   mbchan :- String
   mboxes :- #{String}
   start  :- DateTime]

  Loggable

  (log-level [_] INFO)

  (log-item [_]
    (let [msg (format "Starting `mbsync %s`" (join-mbentry mbchan mboxes))]
      (LogItem. INFO start msg))))

(t/defrecord MbsyncEventStop
  [id      :- Int
   mbchan  :- String
   mboxes  :- #{String}
   start   :- DateTime
   stop    :- DateTime
   level   :- Int
   status  :- Int
   error   :- (maybe String)
   maildir :- (maybe Maildirstore)]

  Loggable

  (log-level [_] level)

  (log-item [_]
    (let [mbarg (join-mbentry mbchan mboxes)
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

(do (alter-meta! #'strict-map->MbsyncEventStop dissoc :private)
    (alter-meta! #'strict-map->MbsyncEventStart dissoc :private))

(defloggable MbsyncUnknownChannelError WARNING
  [id     :- Int
   mbchan :- String]
  (format "Unknown channel: `%s`" mbchan))

(defloggable IDLEEvent NOTICE
  [imap-url :- String]
  (str "IDLE " imap-url))

(defloggable IDLENewMessageEvent INFO
  [n        :- Int
   imap-url :- String]
  (if (= 1 n)
    (str "IDLE: New message in " imap-url)
    (str "IDLE: " n " new messages in " imap-url)))

(defloggable IMAPCommandError ERR
  [type     :- (enum :folder-not-found)
   imap-url :- String]
  (case type
    :folder-not-found (format "IMAP folder %s does not exist!" imap-url)))

(defloggable IMAPShutdownEvent INFO
  [timeout :- Int]
  (format "Waiting up to %s for IMAP disconnection" (human-duration timeout)))

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

(defloggable PendingSyncsEvent INFO
  [action   :- (enum :pool :release)
   sync-req :- MBMap]
  (->> sync-req
       join-mbmap
       (str (if (= action :pool)
              "Delaying syncs: "
              "Releasing pending syncs: "))))

(defloggable TimeJumpEvent WARNING
  [retry        :- Int
   ms-per-retry :- Int]
  (if (pos? retry)
    (format "Connection retry #%d in %s" retry (human-duration (* retry ms-per-retry)))
    "Time jump! Retrying connections up to 3 times in the next 90 seconds."))
