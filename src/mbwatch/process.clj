(ns mbwatch.process
  (:require [clojure.java.io :as io]
            [mbwatch.schema :refer [VOID]]
            [schema.core :as s :refer [enum]])
  (:import (java.io Writer)))

(s/defn spawn :- Process
  "Asynchronously launch a process."
  [& args]
  (let [[args {:keys [in]}] (update-in (split-with string? args)
                                       [1]
                                       (partial apply hash-map))
        cmd ^"[Ljava.lang.String;" (into-array String args)
        proc (.exec (Runtime/getRuntime) cmd)]
    ;; Like fork(2), we should always close the write end of our pipe
    (with-open [out (.getOutputStream proc)]
      (when in (io/copy in out)))
    proc))

(s/defn dump! :- VOID
  "Dump a process's stdout or stderr into writer."
  [proc   :- Process
   stream :- (enum :out :err)
   writer :- Writer]
  (io/copy (case stream
             :out (.getInputStream proc)
             :err (.getErrorStream proc))
           writer))
