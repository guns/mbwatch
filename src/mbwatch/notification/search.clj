(ns mbwatch.notification.search
  (:require [clojure.core.async.impl.protocols :refer [WritePort]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s])
  (:import (mbwatch.types NotifySpec)))

(s/defn search-and-notify! :- VOID
  [notify-spec :- NotifySpec
   notify-cmd  :- String
   log-chan    :- WritePort])
