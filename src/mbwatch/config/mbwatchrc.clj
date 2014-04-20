(ns mbwatch.config.mbwatchrc
  "Configuration from an mbwatchrc configuration file."
  (:require [clojure-ini.core :refer [read-ini]]
            [mbwatch.types :as t]
            [mbwatch.util :refer [parse-ms]]
            [schema.core :as s :refer [Int]])
  (:import (java.io StringReader)))

(def ^:const DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def DEFAULT-OPTIONS
  {:notify-command "notify-send \"$(cat)\""
   :sync-timer-period "900000"
   :connection-period "300000"
   :connection-timeout "2000"
   :imap-socket-timeout "10000"})

(t/defrecord ^:private Mbwatchrc
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
        (update-in [:sync-timer-period] parse-ms)
        (update-in [:connection-period] parse-ms)
        (update-in [:connection-timeout] parse-ms)
        (update-in [:imap-socket-timeout] parse-ms))))
