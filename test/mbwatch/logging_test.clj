(ns mbwatch.logging-test
  (:require [clojure.core.async :refer [<!! >!! chan]]
            [clojure.test :refer [is]]
            [com.stuartsierra.component :as comp]
            [mbwatch.logging :as l]
            [schema.test :as s])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(s/deftest test-log!
  (let [ch (chan)]
    (l/log! ch {})
    (is (instance? DateTime (:timestamp (<!! ch))))))

(s/deftest test-Loggable->LogItem
  (let [loggable (reify l/Loggable
                   (l/log-level [_] 0)
                   (l/log-item [this] (l/->LogItem this "Hello from ->LogItem.")))
        {:keys [level timestamp message]} (l/log-item loggable)]
    (is (zero? level))
    (is (instance? DateTime timestamp))
    (is (= "Hello from ->LogItem." message))))

(s/deftest test-LoggingService
  (let [sink (atom [])
        ch (chan)
        service (comp/start
                  (l/strict-map->LoggingService
                    {:level l/NOTICE
                     :logger (reify l/IItemLogger
                               (log [_ item] (swap! sink conj (:message item))))
                     :log-chan ch
                     :exit-chan nil}))
        values [(LogItem. l/WARNING (DateTime.) "SYN")
                (LogItem. l/NOTICE (DateTime.) "ACK")
                (LogItem. l/DEBUG (DateTime.) "SYN ACK")
                (LogItem. 0 (DateTime.) "FIN")]]
    (doseq [v values] (>!! ch v))
    (comp/stop service)
    (is (= @sink ["SYN" "ACK" "FIN"]))))
