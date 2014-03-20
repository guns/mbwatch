(ns mbwatch.mbsync.events
  (:require [clojure.string :as string]
            [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.logging :refer [ERR Loggable]]
            [mbwatch.util :refer [human-duration]]
            [schema.core :as s :refer [Int maybe]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(s/defn join-mbargs :- String
  [mbchan :- String
   mboxes :- [String]]
  (if (seq mboxes)
    (str mbchan \: (string/join \, mboxes))
    (str mbchan)))

(s/defrecord MbsyncEventStart
  [level  :- Int
   id     :- Long
   mbchan :- String
   mboxes :- [String]
   start  :- DateTime]

  Loggable

  (log-level [_] level)

  (->log [_]
    (let [msg (format "Starting `mbsync %s`" (join-mbargs mbchan mboxes))]
      (LogItem. level start msg))))

(s/defrecord MbsyncEventStop
  [level   :- Int
   id      :- Long
   mbchan  :- String
   mboxes  :- [String]
   start   :- DateTime
   stop    :- DateTime
   status  :- Int
   error   :- (maybe String)
   maildir :- (maybe Maildirstore)]

  Loggable

  (log-level [_] level)

  (->log [_]
    (let [mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (let [buf (StringBuffer.)
                      fail (if (<= level ERR)
                             (format "FAILURE: `mbsync %s` aborted in %s with status %d."
                                     mbarg Δt status)
                             (format "FAILURE: `mbsync %s` terminated after %s with status %d."
                                     mbarg Δt status))]
                  (.append buf fail)
                  (when error
                    (.append buf \newline)
                    (.append buf error))
                  (str buf)))]
      (LogItem. level stop msg))))
