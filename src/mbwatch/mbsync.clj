(ns mbwatch.mbsync
  (:require [clojure.string :as string]
            [mbwatch.process :as process]
            [mbwatch.util :refer [shell-escape]]
            [schema.core :as s]))

(s/defn spawn-sync :- Process
  "Asynchronously launch an mbsync process to sync a single mail channel. The
   config string is passed to mbsync via `cat` and bash's <(/dev/fd) feature
   in order to avoid temporary files."
  [config   :- String
   mb-chan  :- String
   mb-boxes :- [String]]
  (let [sync-arg (shell-escape
                   (if (seq mb-boxes)
                     (str mb-chan \: (string/join \, mb-boxes))
                     (str mb-chan)))]
    (process/spawn
      "bash" "-c" (str "exec mbsync -c <(cat) " sync-arg)
      :in config)))
