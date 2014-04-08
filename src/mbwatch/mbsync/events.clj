(ns mbwatch.mbsync.events
  (:require [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [mbwatch.logging :refer [ERR Loggable WARNING defloggable]]
            [mbwatch.types :as t :refer [StringList]]
            [mbwatch.util :refer [human-duration join-mbargs]]
            [schema.core :refer [Int maybe]])
  (:import (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(t/defrecord ^:public MbsyncEventStart
  [level  :- Int
   id     :- Int
   mbchan :- String
   mboxes :- StringList
   start  :- DateTime]

  Loggable

  (log-level [_] level)

  (log-item [_]
    (let [msg (format "Starting `mbsync %s`" (join-mbargs mbchan mboxes))]
      (LogItem. level start msg))))

(t/defrecord ^:public MbsyncEventStop
  [level   :- Int
   id      :- Int
   mbchan  :- String
   mboxes  :- StringList
   start   :- DateTime
   stop    :- DateTime
   status  :- Int
   error   :- (maybe String)
   maildir :- (maybe Maildirstore)]

  Loggable

  (log-level [_] level)

  (log-item [_]
    (let [mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (let [buf (StringBuilder.)
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

(defloggable MbsyncUnknownChannelError WARNING
  [id     :- Int
   mbchan :- String]
  (format "Unknown channel: `%s`" mbchan))
