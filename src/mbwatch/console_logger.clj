(ns mbwatch.console-logger
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [mbwatch.logging :refer [IItemLogger Loggable]]
            [mbwatch.mbsync :refer [join-mbargs]]
            [mbwatch.util :refer [human-duration]])
  (:import (java.io Writer)
           (mbwatch.logging LogItem)
           (mbwatch.mbsync MbsyncEventStart MbsyncEventStop)
           (org.joda.time DateTime)
           (org.joda.time.format DateTimeFormat DateTimeFormatter)))

;;
;; TTY helpers
;;

(def sgr
  "ANSI SGR codes.
   http://www.inwap.com/pdp10/ansicode.txt
   http://en.wikipedia.org/wiki/ANSI_escape_code#graphics"
  (merge {:reset      "0"  :clear       "0"  :normal "0"
          :bold       "1"  :nobold      "21"
          :dim        "2"  :nodim       "22"
          :italic     "3"  :noitalic    "23"
          :underline  "4"  :nounderline "24"
          :blink      "5"  :noblink     "25"
          :rapidblink "6"
          :reverse    "7"  :noreverse   "27"
          :inverse    "7"  :noinverse   "27"
          :conceal    "8"  :noconceal   "28"
          :strikeout  "9"  :nostrikeout "29"
          :fraktur    "20" :nofraktur   "23"
          :black      "30" :BLACK       "40"
          :red        "31" :RED         "41"
          :green      "32" :GREEN       "42"
          :yellow     "33" :YELLOW      "43"
          :blue       "34" :BLUE        "44"
          :magenta    "35" :MAGENTA     "45"
          :cyan       "36" :CYAN        "46"
          :white      "37" :WHITE       "47"
          :default    "39" :DEFAULT     "49"
          :frame      "51" :noframe     "54"
          :encircle   "52" :noencircle  "54"
          :overline   "53" :nooverline  "55"
          :ideogram-underline           "60"
          :ideogram-double-underline    "61"
          :ideogram-overline            "62"
          :ideogram-double-overline     "63"
          :ideogram-stress              "64"}
         (reduce (fn [m n]
                   (assoc m
                          (keyword (str "fg" n)) (str "38;5;" n)
                          (keyword (str "bg" n)) (str "48;5;" n)))
                 {} (range 256))))

(defn tty-color-count []
  (try
    (Integer/parseInt (re-find #"\d+" (:out (sh "tput" "colors"))))
    (catch Throwable _
      8)))

(def default-colors
  (let [c256 (= (tty-color-count) 256)]
    [[:red :inverse :underline]   ; EMERG
     [:red :inverse]              ; ALERT
     [:red :underline]            ; CRIT
     [:red]                       ; ERR
     [:magenta]                   ; WARNING
     (if c256 [:fg83] [:green])   ; NOTICE
     (if c256 [:fg218] [:yellow]) ; INFO
     (if c256 [:fg81] [:cyan])    ; DEBUG
     ]))

(defn tty? []
  (boolean (System/console)))

(defn sgr-join [styles]
  (string/join \; (mapv sgr styles)))

(defn- ^String wrap [msg sgr-string]
  (str "\033[" sgr-string "m" msg "\033[0m"))

;;
;; Protocols and Types
;;

(extend-protocol Loggable
  MbsyncEventStart
  (->log [this]
    (let [{:keys [level mbchan mboxes start]} this
          msg (format "Starting `mbsync %s`" (join-mbargs mbchan mboxes))]
      (LogItem. level start msg)))

  MbsyncEventStop
  (->log [this]
    (let [{:keys [level mbchan mboxes start stop status error]} this
          mbarg (join-mbargs mbchan mboxes)
          Δt (human-duration start stop)
          msg (if (zero? status)
                (format "Finished `mbsync %s` in %s." mbarg Δt)
                (format "FAILURE: `mbsync %s` aborted in %s with status %d.\n%s" mbarg Δt status error))]
      (LogItem. level stop msg))))

(def ^:private ^DateTimeFormatter timestamp-format
  (DateTimeFormat/forPattern "HH:mm:ss"))

(deftype ConsoleLogger [^Writer writer colors]
  IItemLogger
  (log [this log-item]
    (let [{:keys [level timestamp message]} log-item
          ts (.print timestamp-format ^DateTime timestamp)
          msg (cond-> (str "[" ts "] " message)
                colors (wrap (get colors level)))]
      (.write writer msg)
      (.write writer "\n")
      (.flush writer))))
