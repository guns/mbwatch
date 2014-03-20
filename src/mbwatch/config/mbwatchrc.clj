(ns mbwatch.config.mbwatchrc
  (:require [clojure-ini.core :refer [read-ini]]
            [schema.core :as s])
  (:import (java.io StringReader)))

(def DEFAULT-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/rc"))

(def DEFAULT-OPTIONS
  {:notify-cmd "notify-send \"$(cat)\""})

(s/defrecord Mbwatchrc
  [notify-cmd :- String])

(s/defn parse :- Mbwatchrc
  [s :- String]
  (strict-map->Mbwatchrc
    (merge DEFAULT-OPTIONS (read-ini (StringReader. s) :keywordize? true))))
