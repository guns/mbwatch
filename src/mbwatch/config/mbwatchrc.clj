(ns mbwatch.config.mbwatchrc
  "Configuration from an mbwatchrc configuration file."
  (:require [clojure-ini.core :refer [read-ini]]
            [mbwatch.types :as t]
            [schema.core :as s :refer [Int]])
  (:import (java.io StringReader)))

(def DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def DEFAULT-OPTIONS
  {:notify-cmd "notify-send \"$(cat)\""
   :imap-timeout "10000"})

(t/defrecord ^:private Mbwatchrc
  [notify-cmd   :- String
   imap-timeout :- Int])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (-> DEFAULT-OPTIONS
        (merge (read-ini (StringReader. s) :keywordize? true))
        (update-in [:imap-timeout] #(Long/parseLong %)))))
