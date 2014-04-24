(ns mbwatch.core
  (:require [com.stuartsierra.component :as comp]
            [mbwatch.application-master :refer [->ApplicationMaster]]
            [mbwatch.cli :refer [parse-argv!]])
  (:gen-class))

(defn -main
  "Command line entry point."
  [& argv]
  (try
    (let [options (parse-argv! argv)]
      (case options
        true (System/exit 0)
        false (System/exit 1)
        :else (comp/start (->ApplicationMaster options))))
    (finally
      (shutdown-agents))))
