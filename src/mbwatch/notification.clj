(ns mbwatch.notification
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [mbwatch.config :refer [mdir-path]]
            [mbwatch.maildir :refer [new-messages senders]]
            [mbwatch.mbsync]
            [mbwatch.util :refer [to-ms]]
            [schema.core :as s :refer [maybe]])
  (:import (javax.mail.internet MimeMessage)
           (mbwatch.mbsync MbsyncEventStop)))

(s/defn ^:private format-msg :- (maybe String)
  [messages :- [MimeMessage]]
  (let [n (count messages)]
    (when (pos? n)
      (format "%d new message%s from:\n\n%s"
              n
              (if (= 1 n) "" \s)
              (string/join \newline (senders messages))))))

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
          (let [ts (to-ms start)]
            (->> (sort bs)
                 (mapv #(format "[%s/%s] %s"
                                mbchan % (format-msg
                                           (new-messages (mdir-path maildir %) ts))))
                 (string/join "\n\n"))))))))

(s/defn new-message-notification :- (maybe String)
  [notify-map :- {String #{String}}
   events     :- [MbsyncEventStop]]
  (let [msgs (->> events
                  (map (partial sync-event->notification notify-map))
                  (remove nil?))]
    (when (seq msgs)
      (string/join "\n\n" msgs))))
