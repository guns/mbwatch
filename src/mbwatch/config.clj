(ns mbwatch.config
  (:require [mbwatch.config.mbsyncrc :as mbs]
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
