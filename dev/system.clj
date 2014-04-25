(ns system
  (:require [mbwatch.application-master :refer [->ApplicationMaster]]
            [mbwatch.cli :refer [parse-argv!]]))

(defn system []
  (->ApplicationMaster (parse-argv! [])))
