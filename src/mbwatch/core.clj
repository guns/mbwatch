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
      (case options
        true (System/exit 0)
        false (System/exit 1)
        :else
        (let [master (comp/start (->ApplicationMaster options))
              status ^AtomicBoolean (:status master)]
          (try
            (loop []
              (sig-wait status)
              (when (.get status)
                (recur)))
            (finally (comp/stop master))))))
    (finally
      (shutdown-agents))))
