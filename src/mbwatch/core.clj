(ns mbwatch.core
  (:require [com.stuartsierra.component :as comp]
            [mbwatch.application-master :refer [->ApplicationMaster]]
            [mbwatch.cli :refer [parse-argv!]]
            [mbwatch.concurrent :refer [sig-wait]]
            [mbwatch.util :refer [add-shutdown-hook!]])
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
          (add-shutdown-hook!
            (comp/stop master)) ; STOP ApplicationMaster
          (try
            (while (.get status)
              (sig-wait status))
            (System/exit 0)
            (catch Throwable _
              (System/exit 1))))))
    (finally
      (shutdown-agents))))
