(ns mbwatch.process
  (:require [clojure.java.io :as io]
            [mbwatch.concurrent :refer [first-alt sig-wait]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s :refer [either enum]])
  (:import (java.io File IOException OutputStream Writer)))

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

(s/defn interruptible-wait :- Boolean
  "Run process while waiting on monitor. If a notification is received, the
   process is terminated early and false is returned."
  ([monitor proc]
   (interruptible-wait monitor proc true))
  ([monitor   :- Object
    proc      :- Process
    graceful? :- Boolean]
   (let [wait (future (sig-wait monitor) false)]
     (if (first-alt (.waitFor proc) (deref wait))
       graceful?
       (do (future-cancel wait)
           (.destroy proc)
           (recur monitor proc false))))))

(s/defn dump! :- VOID
  "Dump a process's stdout or stderr into writer."
  [proc   :- Process
   stream :- (enum :out :err)
   writer :- (either OutputStream Writer File)]
  (try
    (io/copy (case stream
               :out (.getInputStream proc)
               :err (.getErrorStream proc))
             writer)
    (catch IOException _)))
