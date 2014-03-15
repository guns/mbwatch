(ns mbwatch.config
  (:require [mbwatch.config.mbsyncrc :as mb]
            [schema.core :as s])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(s/defrecord Config
  [mbsyncrc :- Mbsyncrc])

(s/defn new-config :- Config
  [mbsyncrc-path :- String]
  (strict-map->Config
    {:mbsyncrc (mb/parse (slurp mbsyncrc-path))}))
