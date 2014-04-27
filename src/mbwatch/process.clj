(ns mbwatch.process
  (:require [clojure.java.io :as io]
            [mbwatch.concurrent :refer [sig-wait]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s :refer [Int either enum]])
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
  "Run process while waiting on lock. If a notification is received, the
   process is sent a termination signal and false is returned. If the process
   has not ended within timeout ms, the process is terminated and false is
   returned. A timeout of zero means no timeout."
  [lock    :- Object
   proc    :- Process
   timeout :- Int]
  (let [graceful? (promise)
        ;; Loop so that the user can send multiple termination signals to a
        ;; recalcitrant process.
        sword (future
                (loop []
                  (sig-wait lock timeout)
                  (deliver graceful? false)
                  (.destroy proc)
                  (recur)))]
    (try
      (.waitFor proc)
      (deliver graceful? true)
      @graceful?
      (finally
        (future-cancel sword)))))

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
