(ns system
  (:require [mbwatch.application-master :refer [->ApplicationMaster]]))

(defn system []
  (->ApplicationMaster {}))
