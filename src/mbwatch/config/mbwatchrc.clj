(ns mbwatch.config.mbwatchrc
  "Configuration from an mbwatchrc configuration file."
  (:require [clojure-ini.core :refer [read-ini]]
            [mbwatch.types :as t]
            [schema.core :as s :refer [Int]])
  (:import (java.io StringReader)))

(def ^:const DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def ^:const DEFAULT-OPTIONS
  {:notify-command "notify-send \"$(cat)\""
   :sync-timer-period "900000"
   :connection-period "300000"
   :connection-timeout "2000"
   :imap-socket-timeout "10000"})

(t/defrecord Mbwatchrc
  [notify-command      :- String
   sync-timer-period   :- Int
   connection-period   :- Int
   connection-timeout  :- Int
   imap-socket-timeout :- Int])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (-> DEFAULT-OPTIONS
        (merge (read-ini (StringReader. s) :keywordize? true))
        (update-in [:sync-timer-period] #(Long/parseLong %))
        (update-in [:connection-period] #(Long/parseLong %))
        (update-in [:connection-timeout] #(Long/parseLong %))
        (update-in [:imap-socket-timeout] #(Long/parseLong %)))))
