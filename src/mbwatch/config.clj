(ns mbwatch.config
  "Top level options from all configuration sources."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [get-default-options parse-opts]]
            [mbwatch.config.mbsyncrc :refer [DEFAULT-MBSYNCRC-PATH
                                             parse-mbsyncrc]]
            [mbwatch.connection-watcher :as cw]
            [mbwatch.logging.levels :refer [DEBUG EMERG INFO NAME->LEVEL]]
            [mbwatch.maildir :refer [flatten-mbmap]]
            [mbwatch.mbmap :refer [mbmap-merge mbmap-merge+ parse-mbline]]
            [mbwatch.sync-timer :as st]
            [mbwatch.time :refer [human-duration parse-ms]]
            [mbwatch.types :as t :refer [MBMap MBMap+]]
            [mbwatch.util :refer [istr= parse-kv-string zero-or-min]]
            [schema.core :as s :refer [Any Int validate]])
  (:import (clojure.lang Keyword)
           (mbwatch.config.mbsyncrc Mbsyncrc)))

(def ^:const DEFAULT-MBWATCHRC-PATH
  (str (System/getProperty "user.home") "/.mbwatchrc"))

(def ^:private default-notify-cmd
  (delay
    (if (zero? (:exit (sh "sh" "-c" "command -v notify-send")))
      "notify-send \"$(cat)\""
      "")))

(def config-options
  "Default config options as a tools.cli options vector."
  (let [mbmap? (partial validate MBMap)
        mbmap+? (partial validate MBMap+)
        log-level-desc (format "%d-%d or one of %s"
                               EMERG DEBUG (string/join ", " (keys NAME->LEVEL)))
        file-is-readable [#(.exists (io/file %)) "File does not exist"
                          #(.canRead (io/file %)) "File is unreadable"]
        zero-or-min? (fn [period]
                       [#(zero-or-min % period)
                        (str "Must be zero or >= " (human-duration period))])]
    [["-i" "--idle MBSYNC-ARGS" "Mailboxes to watch with IMAP IDLE"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :assoc-fn (fn [m k v] (update-in m [k] mbmap-merge+ v))
      :validate [mbmap+? "No mailboxes specified"]]
     ["-s" "--sync MBSYNC-ARGS" "Channels to periodically sync"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :assoc-fn (fn [m k v] (update-in m [k] mbmap-merge v))
      :validate [mbmap? "Bad mbsync argument format"]]
     ["-n" "--notify MBSYNC-ARGS" "Mailboxes to notify on"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :assoc-fn (fn [m k v] (update-in m [k] mbmap-merge v))
      :validate [mbmap? "Bad mbsync argument format"]]
     ["-b" "--blacklist MBSYNC-ARGS" "Mailboxes to never notify on"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :assoc-fn (fn [m k v] (update-in m [k] mbmap-merge v))
      :validate [mbmap? "Bad mbsync argument format"]]
     ["-l" "--log-level LEVEL" log-level-desc
      :default INFO
      :default-desc "INFO"
      :parse-fn #(or (NAME->LEVEL %)
                     (when (re-find #"\A\d+\z" %)
                       (Long/parseLong %)))
      :validate [#(<= EMERG % DEBUG) log-level-desc]]
     ["-c" "--config PATH" "Path to mbsyncrc"
      :default DEFAULT-MBSYNCRC-PATH
      :default-desc "~/.mbsyncrc"
      :validate file-is-readable]
     ["-C" "--mbwatch-config PATH" "Path to mbwatch configuration file"
      :default DEFAULT-MBWATCHRC-PATH
      :default-desc "~/.mbwatchrc"
      :validate file-is-readable]
     ["-N" "--notify-cmd SHELL-CMD" "Notification command; receives text on stdin"
      :default @default-notify-cmd]
     ["-S" "--sync-period TIME" "Time between periodic syncs"
      :default (parse-ms "15m")
      :default-desc "15m"
      :parse-fn parse-ms
      :validate (zero-or-min? st/MIN-POS-PERIOD)]
     [nil "--conn-period TIME" "Time between connection checks"
      :default (parse-ms "5m")
      :default-desc "5m"
      :parse-fn parse-ms
      :validate (zero-or-min? cw/MIN-POS-PERIOD)]
     [nil "--conn-timeout TIME" "Timeout for DNS queries and connection checks"
      :default (parse-ms "2s")
      :default-desc "2s"
      :parse-fn parse-ms]
     [nil "--imap-timeout TIME" "IMAP-specific socket timeout"
      :default (parse-ms "10s")
      :default-desc "10s"
      :parse-fn parse-ms]
     [nil "--cache-passwords BOOL" "Cache IMAP passwords from PassCmds"
      :default false
      :parse-fn (or (partial istr= "true")
                    (partial istr= "yes")
                    (partial istr= "on")
                    (partial istr= "1"))]]))

(s/defn ^:private parse-config-file :- {Keyword Any}
  "Parse mbwatch-config-path, returning an option map with defaults. If
   mbwatch-config-path does not exist, only the defaults from config-options
   are returned. Throws an exception if there are any errors."
  [mbwatch-config-path :- Coercions]
  (let [{:keys [options errors]} (if (.exists (io/file mbwatch-config-path))
                                   (-> (slurp mbwatch-config-path)
                                       parse-kv-string
                                       (as-> m
                                         (mapv (fn [[k v]]
                                                 (str "--" (name k) \= v)) m))
                                       (parse-opts config-options))
                                   {:options (get-default-options config-options)})]
    (if errors
      (throw (IllegalArgumentException.
               (format "The following errors occured while parsing %s:\n\n%s\n"
                       mbwatch-config-path
                       (string/join "\n" errors))))
      options)))

(t/defrecord Config
  [mbsyncrc        :- Mbsyncrc
   idle            :- MBMap+
   sync            :- MBMap
   notify          :- MBMap
   blacklist       :- MBMap
   log-level       :- Int
   config          :- String
   mbwatch-config  :- String
   notify-cmd      :- String
   conn-period     :- Int
   sync-period     :- Int
   conn-timeout    :- Int
   imap-timeout    :- Int
   cache-passwords :- Boolean])

(s/defn ->Config :- Config
  "Create a Config object. Throws exceptions on errors."
  [cli-options         :- {Keyword Any}
   mbsyncrc-path       :- Coercions
   mbwatch-config-path :- Coercions]
  (let [mbsyncrc-file (io/file mbsyncrc-path)
        ;; mbsync _requires_ a config file
        _ (when-not (and (.exists mbsyncrc-file) (.canRead mbsyncrc-file))
            (throw (RuntimeException.
                     (str (pr-str mbsyncrc-path)
                          " does not exist, or cannot be read! mbsync requires a config file."))))
        mbsyncrc (parse-mbsyncrc (slurp mbsyncrc-path))
        ;; Parse the config file as if it were an command line argument vector
        options (merge (parse-config-file mbwatch-config-path)
                       cli-options)
        ;; Notify on all IDLE and periodic syncs by default
        options (update-in options [:notify]
                           #(if (seq %)
                              %
                              (mbmap-merge (:idle options) (:sync options))))
        ;; Flatten notify-map and blacklist-map
        options (reduce
                  (fn [m k]
                    (update-in
                      m [k] #(flatten-mbmap % (:mbchan->Maildirstore mbsyncrc))))
                  options [:notify :blacklist])]
    (strict-map->Config
      (assoc options :mbsyncrc mbsyncrc))))
