(ns mbwatch.logging-test
  (:require [clojure.core.async :refer [<!! >!! chan close!]]
            [clojure.test :refer [is]]
            [com.stuartsierra.component :as comp]
            [mbwatch.logging :refer [->LogItem ->LoggingService
                                     log-with-timestamp!]]
            [mbwatch.logging.levels :refer [DEBUG NOTICE WARNING]]
            [mbwatch.logging.protocols :refer [ILogger Loggable log-item]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(deftest test-log-with-timestamp!
  (let [ch (chan)]
    (log-with-timestamp! ch {})
    (is (instance? DateTime (:timestamp (<!! ch))))))

(deftest test-Loggable->LogItem
  (let [loggable (reify Loggable
                   (log-level [_] 0)
                   (log-item [this] (->LogItem this "Hello from ->LogItem.")))
        {:keys [level timestamp message]} (log-item loggable)]
    (is (zero? level))
    (is (instance? DateTime timestamp))
    (is (= "Hello from ->LogItem." message))))

(deftest test-LoggingService
  (let [sink (atom [])
        ch (chan)
        service (comp/start
                  (->LoggingService
                    NOTICE
                    (reify ILogger
                      (log [_ item] (swap! sink conj (:message item))))
                    ch))
        values [(LogItem. WARNING (DateTime.) "SYN")
                (LogItem. NOTICE (DateTime.) "ACK")
                (LogItem. DEBUG (DateTime.) "SYN ACK")
                (LogItem. 0 (DateTime.) "FIN")]]
    (doseq [v values] (>!! ch v))
    (close! ch)
    (comp/stop service)
    (is (= @sink ["SYN" "ACK" "FIN"]))))
