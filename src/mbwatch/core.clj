(ns mbwatch.core
  (:require [com.stuartsierra.component :as comp]
            [mbwatch.application-master :refer [->ApplicationMaster]]
            [mbwatch.cli :refer [parse-argv!]]
            [mbwatch.concurrent :refer [sig-wait]])
  (:import (java.util.concurrent.atomic AtomicBoolean))
  (:gen-class))

(defn -main
  "Command line entry point."
  [& argv]
  (try
    (let [options (parse-argv! argv)]
      (condp = options
        true (System/exit 0)
        false (System/exit 1)
        (let [master (comp/start (->ApplicationMaster options)) ; START ApplicationMaster
              status ^AtomicBoolean (:status master)]
          (try
            (loop []
              (sig-wait status)
              (when (.get status)
                (recur)))
            (finally (comp/stop master)))))) ; STOP ApplicationMaster
    (finally
      (shutdown-agents))))
