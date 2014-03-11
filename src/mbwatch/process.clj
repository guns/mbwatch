(ns mbwatch.process
  (:require [clojure.java.io :as io]
            [mbwatch.types :refer [VOID]]
            [mbwatch.util :refer [first-alt sig-wait]]
            [schema.core :as s :refer [enum]]))

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
  "Run process while waiting on signal. If a notification is received, the
   process is terminated early and false is returned."
  ([signal proc]
   (interruptible-wait signal proc true))
  ([signal    :- Object
    proc      :- Process
    graceful? :- Boolean]
   (let [wait (future (sig-wait signal) false)]
     (if (first-alt (.waitFor proc) (deref wait))
       graceful?
       (do (future-cancel wait)
           (.destroy proc)
           (recur signal proc false))))))

(s/defn dump! :- VOID
  "Dump a process's stdout or stderr into writer."
  [proc   :- Process
   stream :- (enum :out :err)
   writer :- s/Any]
  (io/copy (case stream
             :out (.getInputStream proc)
             :err (.getErrorStream proc))
           writer))
