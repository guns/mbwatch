(ns mbwatch.config
  "Top level options from all configuration sources."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.string :as string]
            [mbwatch.config.mbsyncrc :as mbs :refer [Maildirstore]]
            [mbwatch.types :as t]
            [mbwatch.util :refer [parse-kv-string parse-ms]]
            [schema.core :as s :refer [Int]])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(def ^:const DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/config"))

(def DEFAULT-OPTIONS
  {:notify-command "notify-send \"$(cat)\""
   :sync-timer-period "15m"
   :connection-period "5m"
   :connection-timeout "2s"
   :imap-socket-timeout "10s"})

(t/defrecord ^:private Config
  [mbsyncrc            :- Mbsyncrc
   notify-command      :- String
   sync-timer-period   :- Int
   connection-period   :- Int
   connection-timeout  :- Int
   imap-socket-timeout :- Int])

(s/defn ->Config :- Config
  [mbsyncrc-path       :- Coercions
   mbwatch-config-path :- Coercions]
  (let [mbsyncrc (mbs/parse (slurp mbsyncrc-path))
        mbwatch-config (parse-kv-string (slurp mbwatch-config-path))]
    (strict-map->Config
      (-> {:mbsyncrc mbsyncrc}
          (merge DEFAULT-OPTIONS mbwatch-config)
          (update-in [:sync-timer-period] parse-ms)
          (update-in [:connection-period] parse-ms)
          (update-in [:connection-timeout] parse-ms)
          (update-in [:imap-socket-timeout] parse-ms)))))

(s/defn mdir-path :- String
  [maildir :- Maildirstore
   mbox    :- String]
  (let [{:keys [path inbox flatten]} maildir]
    (cond (= "INBOX" mbox) inbox
          (nil? flatten) (str (io/file path mbox))
          :else (->> (string/split mbox #"/")
                     (string/join flatten)
                     (io/file path)
                     str))))
