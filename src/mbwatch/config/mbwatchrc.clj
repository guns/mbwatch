(ns mbwatch.config.mbwatchrc
  (:require [clojure-ini.core :refer [read-ini]]
            [schema.core :as s])
  (:import (java.io StringReader)))

(def default-path
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def default-options
  {:notification-cmd "notify-send \"$(cat)\""})

(s/defrecord Mbwatchrc
  [notification-cmd :- String])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (merge default-options (read-ini (StringReader. s) :keywordize? true))))
