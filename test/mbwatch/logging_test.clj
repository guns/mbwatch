(ns mbwatch.logging-test
  (:require [clojure.core.async :refer [<!! >!! chan close!]]
            [clojure.test :refer [is]]
            [com.stuartsierra.component :as comp]
            [mbwatch.logging :as l]
            [schema.test :refer [deftest]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(deftest test-log-with-timestamp!
  (let [ch (chan)]
    (l/log-with-timestamp! ch {})
    (is (instance? DateTime (:timestamp (<!! ch))))))

(deftest test-Loggable->LogItem
  (let [loggable (reify l/Loggable
                   (l/log-level [_] 0)
                   (l/log-item [this] (l/->LogItem this "Hello from ->LogItem.")))
        {:keys [level timestamp message]} (l/log-item loggable)]
    (is (zero? level))
    (is (instance? DateTime timestamp))
    (is (= "Hello from ->LogItem." message))))

(deftest test-LoggingService
  (let [sink (atom [])
        ch (chan)
        service (comp/start
                  (l/->LoggingService
                    l/NOTICE
                    (reify l/IItemLogger
                      (log [_ item] (swap! sink conj (:message item))))
                    ch))
        values [(LogItem. l/WARNING (DateTime.) "SYN")
                (LogItem. l/NOTICE (DateTime.) "ACK")
                (LogItem. l/DEBUG (DateTime.) "SYN ACK")
                (LogItem. 0 (DateTime.) "FIN")]]
    (doseq [v values] (>!! ch v))
    (close! ch)
    (comp/stop service)
    (is (= @sink ["SYN" "ACK" "FIN"]))))
