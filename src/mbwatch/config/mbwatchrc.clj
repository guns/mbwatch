(ns mbwatch.config.mbwatchrc
  (:require [clojure-ini.core :refer [read-ini]]
            [schema.core :as s :refer [Int]])
  (:import (java.io StringReader)))

(def DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def DEFAULT-OPTIONS
  {:notify-cmd "notify-send \"$(cat)\""
   :imap-timeout "10000"})

(s/defrecord Mbwatchrc
  [notify-cmd   :- String
   imap-timeout :- Int])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (-> DEFAULT-OPTIONS
        (merge (read-ini (StringReader. s) :keywordize? true))
        (update-in [:imap-timeout] #(Long/parseLong %)))))
