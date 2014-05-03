(ns mbwatch.logging-test
  (:require [clojure.core.async :refer [<!! >!! chan close!]]
            [clojure.test :refer [is]]
            [com.stuartsierra.component :as comp]
            [mbwatch.logging :refer [->LogItem ->LoggingService defloggable
                                     log-with-timestamp!]]
            [mbwatch.logging.levels :refer [DEBUG NOTICE WARNING]]
            [mbwatch.logging.protocols :refer [ILogger Loggable log-item
                                               log-level]]
            [schema.test :refer [deftest]])
  (:import (clojure.lang Keyword)
           (mbwatch.logging LogItem)
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

(defloggable TestRecord DEBUG
  [type :- Keyword]
  (case type
    :foo "FOO"))

(deftest test-defloggable
  (let [r (->TestRecord :foo)]
    (is (instance? DateTime (:timestamp r)))
    (is (= (log-level r) DEBUG))
    (is (= "FOO" (:message (log-item r))))))

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
