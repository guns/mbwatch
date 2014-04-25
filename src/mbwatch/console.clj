(ns mbwatch.console
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [mbwatch.logging :refer [IItemLogger]]
            [schema.core :as s :refer [Any Int either maybe]])
  (:import (clojure.lang Keyword)
           (java.io BufferedReader InputStreamReader)
           (org.joda.time DateTime)
           (org.joda.time.format DateTimeFormat DateTimeFormatter)))

(def ^DateTimeFormatter TIMESTAMP-FORMAT
  (DateTimeFormat/forPattern "HH:mm:ss"))

(def ^DateTimeFormatter MILLIS-TIMESTAMP-FORMAT
  (DateTimeFormat/forPattern "HH:mm:ss.SSS"))

(def SGR
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

(s/defn tty-color-count :- Int
  []
  (try
    (Integer/parseInt (re-find #"\d+" (:out (sh "tput" "colors"))))
    (catch Throwable _
      8)))

(s/defn get-default-colors :- [[Keyword]]
  []
  (let [c256 (= (tty-color-count) 256)]
    [[:red :inverse :bold]        ; EMERG
     [:red :inverse]              ; ALERT
     [:red :bold]                 ; CRIT
     [:red]                       ; ERR
     [:magenta]                   ; WARNING
     (if c256 [:fg83] [:green])   ; NOTICE
     (if c256 [:fg218] [:yellow]) ; INFO
     (if c256 [:fg81] [:cyan])    ; DEBUG
     ]))

(s/defn tty? :- Boolean
  []
  (boolean (System/console)))

(s/defn ^:private sgr-join :- String
  [styles :- (either String [Any])]
  (if (string? styles)
    styles
    (string/join \; (mapv #(if (keyword? %) (SGR %) %) styles))))

(s/defn ^:private wrap :- String
  [msg        :- Object
   sgr-string :- (maybe String)]
  (if (seq sgr-string)
    (str "\033[" sgr-string "m" msg "\033[0m")
    msg))

(deftype ConsoleLogger [^Appendable stream colors ^DateTimeFormatter dt-formatter]

  IItemLogger

  (log [_ log-item]
    (let [{:keys [level timestamp message]} log-item
          ts (.print dt-formatter ^DateTime timestamp)
          msg (wrap (str "[" ts "] " message) (get colors level))]
      (.append stream msg)
      (.append stream \newline))))

(s/defn ->ConsoleLogger :- ConsoleLogger
  ([stream]
   (->ConsoleLogger stream (get-default-colors) TIMESTAMP-FORMAT))
  ([stream colors]
   (->ConsoleLogger stream colors TIMESTAMP-FORMAT))
  ([stream       :- Appendable
    colors       :- [Any]
    dt-formatter :- DateTimeFormatter]
   (ConsoleLogger. stream (mapv sgr-join colors) dt-formatter)))

(s/defn console-reader :- BufferedReader
  "Returns System/in as a buffered character reader."
  []
  (BufferedReader. (InputStreamReader. System/in "UTF-8")))

(defmacro with-read-line
  "Execute body with input line from rdr-sym bound to line-sym. Reading from a
   BufferedReader is uninterruptible; this macro wraps the .readLine call in a
   future and ensures it is interrupted before exiting."
  [rdr-sym line-sym & body]
  `(let [f# (future (.readLine ~rdr-sym))]
     (try
       (let [~line-sym @f#]
         ~@body)
       (catch InterruptedException _#) ; We are expecting this
       (finally
         (future-cancel f#)))))

(defmacro with-console-input
  "Execute body repeatedly with input line bound to line-sym. Returns
   immediately if System/in is not a console. Exits if input line is nil
   (stream closed) or if the body returns nil."
  {:requires [BufferedReader console-reader tty?]}
  [line-sym & body]
  `(when (tty?)
     (let [rdr# ^BufferedReader (console-reader)]
       (loop []
         (when (some? (with-read-line rdr# ~line-sym
                        (when (some? ~line-sym)
                          (do ~@body))))
           (recur))))))
