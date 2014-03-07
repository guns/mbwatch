(ns mbwatch.mbsync
  (:require [clojure.string :as string]
            [mbwatch.process :as process]
            [mbwatch.util :refer [shell-escape]]
            [schema.core :as s :refer [defschema pair]])
  (:refer-clojure :exclude [sync]))

(defschema MbsyncRequest
  (pair String "mbsync channel"
        [String] "mbsync boxes"))

(s/defn sync :- Process
  "Asynchronously launch an mbsync process to sync a single mail channel. The
   config string is passed to mbsync via `cat` and bash's <(/dev/fd) feature
   in order to avoid temporary files."
  [config  :- String
   request :- MbsyncRequest]
  (let [[ch bs] request
        sync-arg (shell-escape (if (seq bs)
                                 (str ch \: (string/join \, bs))
                                 (str ch)))]
    (process/spawn
      "bash" "-c" (str "exec mbsync -c <(cat) " sync-arg)
      :in config)))
