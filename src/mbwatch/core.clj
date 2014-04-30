(ns mbwatch.core
  (:require [com.stuartsierra.component :as comp]
            [mbwatch.application-master :refer [->ApplicationMaster]]
            [mbwatch.cli :refer [parse-argv!]]
            [mbwatch.concurrent :refer [sig-wait]]
            [mbwatch.console :refer [print-console]]
            [mbwatch.logging.levels :refer [EMERG]]
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
          (while (.get status)
            (sig-wait status))
          (System/exit 0))))
    (catch Throwable e
      (print-console EMERG :err (str e))
      (System/exit 1))
    (finally
      (shutdown-agents))))
