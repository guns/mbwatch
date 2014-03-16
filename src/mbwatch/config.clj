(ns mbwatch.config
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [mbwatch.config.mbsyncrc :as mbs :refer [Maildirstore]]
            [mbwatch.config.mbwatchrc :as mbw]
            [schema.core :as s])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)
           (mbwatch.config.mbwatchrc Mbwatchrc)))

(s/defrecord Config
  [mbsyncrc  :- Mbsyncrc
   mbwatchrc :- Mbwatchrc])

(s/defn new-config :- Config
  [mbsyncrc-path  :- String
   mbwatchrc-path :- String]
  (strict-map->Config
    {:mbsyncrc (mbs/parse (slurp mbsyncrc-path))
     :mbwatchrc (mbw/parse (slurp mbwatchrc-path))}))

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
