(ns mbwatch.config
  "Top level options from all configuration sources."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.string :as string]
            [mbwatch.config.mbsyncrc :as mbs :refer [Maildirstore]]
            [mbwatch.config.mbwatchrc :as mbw]
            [mbwatch.types :as t]
            [schema.core :as s])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)
           (mbwatch.config.mbwatchrc Mbwatchrc)))

(t/defrecord Config
  [mbsyncrc  :- Mbsyncrc
   mbwatchrc :- Mbwatchrc])

(s/defn ->Config :- Config
  [mbsyncrc-path  :- Coercions
   mbwatchrc-path :- Coercions]
  (Config. (mbs/parse (slurp mbsyncrc-path))
           (mbw/parse (slurp mbwatchrc-path))))

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
