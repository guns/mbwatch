(ns mbwatch.config
  "Top level options from all configuration sources."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [mbwatch.config.mbsyncrc :refer [Maildirstore parse-mbsyncrc]]
            [mbwatch.connection-watcher :as cw]
            [mbwatch.mbmap :refer [parse-mbargs]]
            [mbwatch.shellwords :refer [shell-split]]
            [mbwatch.sync-timer :as st]
            [mbwatch.time :refer [parse-ms]]
            [mbwatch.types :as t :refer [MBMap]]
            [mbwatch.util :refer [parse-kv-string zero-or-min]]
            [schema.core :as s :refer [Any Int validate]])
  (:import (clojure.lang Keyword)
           (mbwatch.config.mbsyncrc Mbsyncrc)))

(def ^:const DEFAULT-CONFIG-PATH
  (str (System/getProperty "user.home") "/.config/mbwatch/config"))

(s/defn ^:private parse-mbline :- MBMap
  [s :- String]
  (parse-mbargs (shell-split s)))

(s/defn config-options :- [[Any]]
  "Default config options as a tools.cli options vector."
  []
  (let [mbmap? (partial validate MBMap)
        notify-cmd (if (zero? (:exit (sh "sh" "-c" "command -v notify-send")))
                     "notify-send \"$(cat)\""
                     "")]
    [["-i" "--idle MBSYNC-ARGS" "Mailboxes to watch with IMAP IDLE"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :validate [mbmap? "Bad mbsync argument format"
                 #(every? seq (vals %)) "No mailboxes specified"]]
     ["-s" "--sync MBSYNC-ARGS" "Channels to periodically sync"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :validate [mbmap? "Bad mbsync argument format"]]
     ["-n" "--notify MBSYNC-ARGS" "Mailboxes to notify on"
      :default {}
      :default-desc ""
      :parse-fn parse-mbline
      :validate [mbmap? "Bad mbsync argument format"
                 #(every? seq (vals %)) "No mailboxes specified"]]
     ["-c" "--notify-cmd SHELL-CMD" "Command that receives notification text on stdin"
      :default notify-cmd
      :default-desc ""]
     ["-C" "--conn-period TIME" "Time between connection checks"
      :default (parse-ms "5m")
      :default-desc "5m"
      :parse-fn parse-ms
      :validate [#(zero-or-min % cw/MIN-POS-PERIOD) (str "Must be zero or >= " cw/MIN-POS-PERIOD)]]
     ["-S" "--sync-period TIME" "Time between periodic syncs"
      :default (parse-ms "15m")
      :default-desc "15m"
      :parse-fn parse-ms
      :validate [#(zero-or-min % st/MIN-POS-PERIOD) (str "Must be zero or >= " st/MIN-POS-PERIOD)]]
     [nil "--conn-timeout TIME" "Timeout for DNS queries and connection checks"
      :default (parse-ms "2s")
      :default-desc "2s"
      :parse-fn parse-ms]
     [nil "--imap-timeout TIME" "IMAP-specific socket timeout"
      :default (parse-ms "10s")
      :default-desc "10s"
      :parse-fn parse-ms]]))

(t/defrecord ^:private Config
  [mbsyncrc     :- Mbsyncrc
   idle         :- MBMap
   sync         :- MBMap
   notify       :- MBMap
   notify-cmd   :- String
   conn-period  :- Int
   sync-period  :- Int
   conn-timeout :- Int
   imap-timeout :- Int])

(s/defn ->Config :- Config
  [user-options        :- {Keyword Any}
   mbsyncrc-path       :- Coercions
   mbwatch-config-path :- Coercions]
  (let [mbsyncrc (parse-mbsyncrc (slurp mbsyncrc-path))
        ;; Parse the config file as if it were an command line argument vector
        {:keys [options errors]}
        (-> (slurp mbwatch-config-path)
            parse-kv-string
            (as-> m (mapv (fn [[k v]] (str "--" (name k) \= v)) m))
            (parse-opts (config-options)))
        idle (:idle options)
        options (-> options
                    ;; Periodically sync idle channels unless specified
                    (update-in [:sync] #(if (seq %)
                                          %
                                          (zipmap (keys idle) (repeat #{}))))
                    ;; Always notify on IDLE mboxes
                    (update-in [:notify] mbmap-merge idle))]
    (if errors
      (throw (IllegalArgumentException.
               (format "The following errors occured while parsing %s:\n%s\n"
                       mbwatch-config-path
                       (string/join "\n" errors))))
      (strict-map->Config
        (merge {:mbsyncrc mbsyncrc} options user-options)))))

(s/defn mdir-path :- String
  [maildir :- Maildirstore
   mbox    :- String]
  (let [{:keys [path inbox flatten]} maildir]
    (cond (= "INBOX" mbox) inbox
          (nil? flatten) (str (io/file path mbox))
          :else (->> (string/split mbox #"/")
                     (string/join flatten)
                     (io/file path)
                     str))))
