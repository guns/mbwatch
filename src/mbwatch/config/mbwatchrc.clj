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
   :imap-socket-timeout "10000"})

(t/defrecord ^:private Mbwatchrc
  [notify-command      :- String
   imap-socket-timeout :- Int])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (-> DEFAULT-OPTIONS
        (merge (read-ini (StringReader. s) :keywordize? true))
        (update-in [:imap-socket-timeout] #(Long/parseLong %)))))
