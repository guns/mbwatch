(ns mbwatch.console
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [mbwatch.logging.levels :refer [WARNING]]
            [mbwatch.logging.protocols :refer [ILogger]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s :refer [Any Int either enum maybe]])
  (:import (clojure.lang Keyword)
           (java.io BufferedReader ByteArrayOutputStream InputStreamReader
                    PrintStream)
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

(s/defn tty? :- Boolean
  []
  (boolean (System/console)))

(s/defn get-default-colors :- [[Keyword]]
  ([]
   (get-default-colors (tty-color-count)))
  ([color-count :- Int]
   (let [c256 (= color-count 256)]
     [[:red :inverse :bold]        ; EMERG
      [:red :inverse]              ; ALERT
      [:red :bold]                 ; CRIT
      [:red]                       ; ERR
      [:magenta]                   ; WARNING
      (if c256 [:fg83] [:green])   ; NOTICE
      (if c256 [:fg218] [:yellow]) ; INFO
      (if c256 [:fg81] [:cyan])    ; DEBUG
      ])))

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

(def ^:private simple-colors
  (when (tty?)
    (mapv sgr-join (get-default-colors 8))))

(s/defn print-console :- VOID
  "Unsynchronized, direct print to System/out and System/err."
  ([msg]
   (print-console nil :out msg))
  ([stream msg]
   (print-console nil stream msg))
  ([level  :- (maybe Int)
    stream :- (enum :out :err)
    msg    :- String]
   (let [os (if (= stream :out) System/out System/err)]
     (.append os "\r")
     (.println os (if level
                    (wrap msg (get simple-colors level))
                    msg)))))

(defmacro catch-print
  {:requires [print-console]}
  [& body]
  `(try
     ~@body
     (catch InterruptedException e#
       (print-console ~WARNING :err (str e#)))
     (catch Throwable e#
       (with-open [os# (new ~ByteArrayOutputStream)
                   ps# (new ~PrintStream os# true)]
         (.printStackTrace e# ps#)
         (print-console ~WARNING :err (str os#))))))

(deftype ConsoleLogger [^Appendable stream colors ^DateTimeFormatter dt-formatter]

  ILogger

  (log [_ log-item]
    (let [{:keys [level ^DateTime timestamp message]} log-item
          ts (.print dt-formatter timestamp)
          ;; Leading CR is to overwrite the rlwrap prompt
          msg (wrap (str "\r[" ts "] " message "\n") (get colors level))]
      (.append stream msg))))

(s/defn ->ConsoleLogger :- ConsoleLogger
  ([stream]
   (->ConsoleLogger stream (get-default-colors) TIMESTAMP-FORMAT))
  ([stream colors]
   (->ConsoleLogger stream colors TIMESTAMP-FORMAT))
  ([stream       :- Appendable
    colors       :- [Any]
    dt-formatter :- DateTimeFormatter]
   (ConsoleLogger. stream (when (tty?) (mapv sgr-join colors)) dt-formatter)))

(s/defn console-reader :- BufferedReader
  "Returns System/in as a buffered character reader."
  []
  (BufferedReader. (InputStreamReader. System/in "UTF-8")))

(defmacro with-read-line
  "Execute body with input line from rdr-sym bound to line-sym.

   Reading from an input stream is normally uninterruptible; this macro wraps
   the .readLine call in a future to ensure that we can at least unblock the
   calling thread. While the future wrapping .readLine will be cancelled,
   the actual read thread will stay alive until it receives input or the
   underlying source is closed, raising an IOException."
  {:requires [BufferedReader]}
  [[line-sym rdr] & body]
  `(let [f# (future (.readLine ^BufferedReader ~rdr))]
     (try
       (let [~line-sym @f#]
         ~@body)
       (finally
         (future-cancel f#)))))

(defmacro with-reader-input
  "Execute body repeatedly with reader input line bound to line-sym. Exits if
   input line is nil (stream closed) or if the body returns nil."
  {:requires [BufferedReader]}
  [[line-sym rdr] & body]
  `(let [^BufferedReader rdr# ~rdr]
     (loop []
       (when (some? (with-read-line [~line-sym rdr#]
                      (when (some? ~line-sym)
                        (do ~@body))))
         (recur)))))
