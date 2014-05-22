(ns mbwatch.config.mbsyncrc
  "Configuration from an mbsyncrc configuration file."
  (:require [clojure.string :as string]
            [mbwatch.concurrent :refer [synchronized-sh]]
            [mbwatch.console :refer [print-console]]
            [mbwatch.logging.levels :refer [CRIT]]
            [mbwatch.posix :refer [expand-user-path parse-passwd]]
            [mbwatch.types :as t :refer [FilteredLine LowerCaseWord Word
                                         strict-map->IMAPCredential
                                         strict-map->Maildirstore tuple]]
            [mbwatch.util :refer [chomp dequote istr=]]
            [schema.core :as s :refer [defschema either enum eq one
                                       optional-key]])
  (:import (clojure.lang IFn)
           (mbwatch.posix Passwd)
           (mbwatch.types IMAPCredential Maildirstore)))

(def ^:const DEFAULT-MBSYNCRC-PATH
  "Default path of mbsyncrc."
  (str (System/getProperty "user.home") \/ ".mbsyncrc"))

(def ^:private ^:const DEFAULT-MBSYNC-INBOX
  (str (System/getProperty "user.home") \/ "Maildir"))

(def ^:private ^:const IMAP-PORT 143)
(def ^:private ^:const IMAPS-PORT 993)

(defschema ^:private Entry
  (tuple LowerCaseWord FilteredLine))

(defschema ^:private MapSectionToken
  [(one (enum :imapstore :maildirstore :channel) "section type")
   (one Word "section name")
   (one [Entry] "config key-value entries")])

(defschema ^:private GeneralSectionToken
  [(one (eq :general) "section type")
   (one (eq nil) "nil")
   (one [FilteredLine] "general config lines")])

(defschema ^:private Token
  (either GeneralSectionToken MapSectionToken))

(defschema ^:private MapSectionValue
  {Word {LowerCaseWord FilteredLine}})

(defschema ^:private Sections
  {(optional-key :general)      [FilteredLine]
   (optional-key :imapstore)    MapSectionValue
   (optional-key :maildirstore) MapSectionValue
   (optional-key :channel)      MapSectionValue})

(s/defn ^:private paragraphs :- [[String]]
  [s :- String]
  (map string/split-lines (string/split s #"\n(\s*\n)+")))

(s/defn ^:private filter-paragraph :- [FilteredLine]
  [p :- [String]]
  (->> p
       (map string/trim)
       (remove #(or (empty? %) (= (first %) \#)))))

(s/defn ^:private par-split :- [[FilteredLine]]
  [s :- String]
  (->> (paragraphs s)
       (map filter-paragraph)
       (remove empty?)))

(s/defn ^:private split-config-line :- Entry
  [line :- FilteredLine]
  (let [[k v] (string/split line #"\s+" 2)]
    [(string/lower-case k) v]))

(s/defn ^:private tokenize-paragraph :- Token
  [p :- [FilteredLine]]
  (let [[l & ls] p
        [section-type section-name] (split-config-line l)]
    (case section-type
      "imapstore"    [:imapstore    section-name (map split-config-line ls)]
      "maildirstore" [:maildirstore section-name (map split-config-line ls)]
      "channel"      [:channel      section-name (map split-config-line ls)]
      [:general nil p])))

(s/defn ^:private tokenize :- [Token]
  [s :- String]
  (map tokenize-paragraph (par-split s)))

(s/defn ^:private parse-tokens :- Sections
  [tokens :- [Token]]
  (reduce
    (fn [m [stype sname sbody]]
      (if (= stype :general)
        (update-in m [:general] #(into (or % []) sbody))
        (update-in m [stype sname] merge (apply hash-map (apply concat sbody)))))
    {} tokens))

(s/defn ^:private parse-credentials :- IMAPCredential
  "Extract credentials from an IMAPStore key-value map. Plaintext passwords
   (bad user! bad!) are stored as a byte-array to prevent leaking via
   stacktraces."
  [imap   :- {LowerCaseWord FilteredLine}
   passwd :- {String Passwd}]
  (let [v (comp dequote imap)
        ;; Only disable SSL if the user insists
        secure? (not (and (istr= (imap "useimaps") "no")
                          (istr= (imap "requiressl") "no")))]
    (-> (cond-> {:ssl? secure? :cert nil}
          (imap "host") (assoc :host (v "host"))
          (imap "port") (assoc :port (Integer/parseInt (v "port")))
          (imap "user") (assoc :user (v "user"))
          (imap "pass") (assoc :pass (.getBytes ^String (v "pass")))
          (imap "passcmd") (assoc :pass (v "passcmd")) ; PassCmd is preferred
          (imap "certificatefile") (assoc :cert (expand-user-path
                                                  passwd (v "certificatefile"))))
        (update-in [:user] #(or % (System/getProperty "user.name")))
        (update-in [:port] #(or % (if (= (imap "useimaps") "no") IMAP-PORT IMAPS-PORT)))
        strict-map->IMAPCredential)))

(s/defn ^:private map-credentials :- {Word IMAPCredential}
  "Extract credentials from IMAPStore sections."
  [imapstores :- MapSectionValue
   passwd     :- {String Passwd}]
  (reduce-kv
    (fn [m store-name imap]
      (assoc m store-name (parse-credentials imap passwd)))
    {} imapstores))

(s/defn ^:private map-maildirstores :- {Word Maildirstore}
  [stores :- MapSectionValue
   passwd :- {String Passwd}]
  (reduce-kv
    (fn [m store-name mdirmap]
      (assoc m store-name
             (strict-map->Maildirstore
               {:inbox (expand-user-path
                         passwd (or (mdirmap "inbox") DEFAULT-MBSYNC-INBOX))
                :path (expand-user-path
                        passwd (mdirmap "path"))
                :flatten (mdirmap "flatten")})))
    {} stores))

(s/defn ^:private get-store-name :- String
  [s :- String]
  (second (re-find #"\A:([^:]+):" s)))

(s/defn ^:private map-master-credentials :- {Word IMAPCredential}
  [channel-section          :- MapSectionValue
   imapname->IMAPCredential :- {Word IMAPCredential}]
  (reduce-kv
    (fn [m ch-name ch-map]
      (if-some [cred (->> (ch-map "master")
                          get-store-name
                          imapname->IMAPCredential)]
        (assoc m ch-name cred)
        m))
    {} channel-section))

(s/defn ^:private map-slave-maildirstores :- {Word Maildirstore}
  [channel-section :- MapSectionValue
   maildirstores   :- {Word Maildirstore}]
  (reduce-kv
    (fn [m ch-name ch-map]
      (if-some [mdir (->> (ch-map "slave")
                          get-store-name
                          maildirstores)]
        (assoc m ch-name mdir)
        m))
    {} channel-section))

(s/defn get-password :- String
  "Get a password from an IMAPCredential :pass field. Calls `sh` to get the
   output of PassCmd. Writes to System/err and returns an empty string when a
   PassCmd returns with a non-zero status."
  [pass :- (either String bytes)]
  (if (string? pass)
    (let [{:keys [exit out err]} (synchronized-sh "sh" "-c" pass)]
      (if (zero? exit)
        (chomp out)
        (do (print-console CRIT :err err)
            "")))
    (String. ^bytes pass)))

(s/defn ^:private replace-passcmd :- MapSectionValue
  [imapstore                :- MapSectionValue
   imapname->IMAPCredential :- {Word IMAPCredential}]
  (reduce-kv
    (fn [m k v]
      (let [pass (pr-str (get-password (get-in imapname->IMAPCredential [k :pass])))]
        (assoc m k (-> v
                       (dissoc "passcmd")
                       (assoc "pass" pass)))))
    {} imapstore))

(s/defn ^:private render-section :- [String]
  [sections :- Sections
   type     :- (:schema (first MapSectionToken))]
  (mapcat (fn [[v kvs]]
            (-> [(str (name type) \space v)]
                (into (mapv (partial string/join \space) (sort-by key kvs)))
                (conj "")))
          (sort-by key (sections type))))

(s/defn ^:private render :- String
  [sections                 :- Sections
   imapname->IMAPCredential :- {Word IMAPCredential}]
  (let [s (update-in sections [:imapstore] replace-passcmd imapname->IMAPCredential)]
    (string/join \newline (concat (:general s)
                                  [""]
                                  (render-section s :imapstore)
                                  (render-section s :maildirstore)
                                  (render-section s :channel)))))

(t/defrecord Mbsyncrc
  [render-fn              :- IFn
   sections               :- Sections
   mbchans                :- #{Word}
   mbchan->Maildirstore   :- {Word Maildirstore}
   mbchan->IMAPCredential :- {Word IMAPCredential}])

(s/defn parse-mbsyncrc :- Mbsyncrc
  [s :- String]
  (let [passwd (parse-passwd (slurp "/etc/passwd"))
        sections (parse-tokens (tokenize s))
        imapname->IMAPCredential (map-credentials (:imapstore sections) passwd)
        mbchan->IMAPCredential (map-master-credentials
                                 (:channel sections)
                                 imapname->IMAPCredential)
        mbchan->Maildirstore (map-slave-maildirstores
                               (:channel sections)
                               (map-maildirstores (:maildirstore sections) passwd))]
    (strict-map->Mbsyncrc
      {:render-fn #(render sections imapname->IMAPCredential)
       :sections sections
       :mbchans (-> sections :channel keys set)
       :mbchan->Maildirstore mbchan->Maildirstore
       :mbchan->IMAPCredential mbchan->IMAPCredential})))
