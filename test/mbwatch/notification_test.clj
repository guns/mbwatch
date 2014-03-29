(ns mbwatch.notification-test
  (:require [clojure.test :refer [is]]
            [mbwatch.logging :refer [DEBUG]]
            [mbwatch.mbsync.events :as e]
            [mbwatch.notification :as n]
            [schema.test :as s])
  (:import (org.joda.time DateTime)))

;; FIXME: Add tests
(s/deftest test-NewMessageNotificationService)
