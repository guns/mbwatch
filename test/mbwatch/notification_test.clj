(ns mbwatch.notification-test
  (:require [clojure.core.async :refer [<!! chan close!]]
            [clojure.test :refer [is]]
            [com.stuartsierra.component :as comp]
            [mbwatch.notification :refer [->NewMessageNotificationService]]
            [mbwatch.types :refer [strict-map->NotifySpec]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.notification NewMessageNotificationService)))

(defn new-notify-spec []
  (strict-map->NotifySpec {:strategy :all
                           :blacklist {}
                           :whitelist {}
                           :references #{}
                           :patterns #{}}))

(deftest test-NewMessageNotificationService
  (let [log-chan-in (chan 0xff)
        notify-service (comp/start
                         (->NewMessageNotificationService
                           "cat" (new-notify-spec) {} log-chan-in))
        log-chan-out (:log-chan-out notify-service)]
    (is (instance? NewMessageNotificationService (<!! log-chan-out))
        "starts with a Lifecycle event")
    (close! log-chan-in)
    (comp/stop notify-service)
    (is (instance? NewMessageNotificationService (<!! log-chan-out))
        "ends with a Lifecycle event")))
