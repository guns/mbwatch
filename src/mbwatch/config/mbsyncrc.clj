(ns mbwatch.config.mbsyncrc
  "Configuration from an mbsyncrc configuration file."
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [mbwatch.passwd :refer [expand-user-path parse-passwd]]
            [mbwatch.types :as t]
            [mbwatch.util :refer [chomp dequote]]
            [schema.core :as s :refer [both defschema either enum eq maybe
                                       one optional-key pair pred]])
  (:import (clojure.lang IPersistentSet)))

(def ^:const DEFAULT-PATH
  "Default path of mbsyncrc."
  (str (System/getProperty "user.home") \/ ".mbsyncrc"))

(def ^:private ^:const DEFAULT-MBSYNC-INBOX
  (str (System/getProperty "user.home") \/ "Maildir"))

(defschema ^:private Word
  (pred #(and (string? %)
              (seq %)
              (not (re-find #"\s" %)))))

(defschema ^:private LowerCaseWord
  (both Word (pred #(not (re-find #"\p{Lu}" %)))))

(defschema ^:private FilteredLine
  (pred #(and (string? %)
              (not (re-seq #"\n|\A\s*#|\A\s*\z|\A\s|\s\z" %)))
        "single non-comment line with no surrounding whitespace"))

(defschema ^:private Entry
  (pair LowerCaseWord "name"
        FilteredLine "value"))

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

(defschema ^:private PortNumber
  (pred #(and (integer? %) (< 0 % 0x1000))))

(defschema ^:private IMAPCredentials
  {:host String
   :port PortNumber
   :user String
   :pass String})

(defschema Maildirstore
  {:inbox   FilteredLine
   :path    FilteredLine
   :flatten (maybe FilteredLine)})

(t/defrecord ^:private Mbsyncrc
  [text                    :- String
   sections                :- Sections
   channels                :- IPersistentSet
   channels->maildirstores :- {Word Maildirstore}])

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
        (update-in m [:general] #(into (or %1 []) %2) sbody)
        (update-in m [stype sname] merge (apply hash-map (apply concat sbody)))))
    {} tokens))

(s/defn ^:private parse-credentials :- IMAPCredentials
  "Extract credentials from an IMAPStore key-value map. Shells out for
   evaluation of PassCmd."
  [imap :- {LowerCaseWord FilteredLine}]
  (let [v (comp dequote imap)]
    (-> (cond-> {}
          (imap "host") (assoc :host (v "host"))
          (imap "port") (assoc :port (Integer/parseInt (v "port")))
          (imap "user") (assoc :user (v "user"))
          (imap "pass") (assoc :pass (v "pass"))
          (imap "passcmd") (assoc :pass (chomp (:out (sh "sh" "-c" (v "passcmd"))))))
        (update-in [:user] #(or % (System/getProperty "user.name")))
        (update-in [:port] #(or % (if (= (imap "useimaps") "no") 143 993))))))

(s/defn ^:private map-credentials :- {Word IMAPCredentials}
  "Extract all credentials from IMAPStore sections.

   Running this in parallel minimizes blocking IO, but also causes a pinentry
   storm if multiple PassCmds call out to gpg.

   https://bugs.gnupg.org/gnupg/issue1109"
  [imapstores :- MapSectionValue]
  (reduce-kv
    (fn [m store-name imap]
      (assoc m store-name (parse-credentials imap)))
    {} imapstores))

(s/defn ^:private map-maildirstores :- {Word Maildirstore}
  [stores :- MapSectionValue]
  (let [passwd-map (parse-passwd (slurp "/etc/passwd"))]
    (reduce-kv
      (fn [m store-name mdirmap]
        (assoc m store-name
               {:inbox (expand-user-path
                         passwd-map (or (mdirmap "inbox") DEFAULT-MBSYNC-INBOX))
                :path (expand-user-path
                        passwd-map (mdirmap "path"))
                :flatten (mdirmap "flatten")}))
      {} stores)))

(s/defn ^:private map-slave-maildirstores :- {Word Maildirstore}
  [channels      :- MapSectionValue
   maildirstores :- {Word Maildirstore}]
  (reduce-kv
    (fn [m ch-name ch-map]
      (if-let [mdir (->> (ch-map "slave")
                         (re-find #"\A:([^:]+):")
                         second
                         maildirstores)]
        (assoc m ch-name mdir)
        m))
    {} channels))

(s/defn ^:private replace-passcmd :- MapSectionValue
  [imapstore                    :- MapSectionValue
   imapstore-names->credentials :- {Word IMAPCredentials}]
  (reduce-kv
    (fn [m k v]
      (let [pass (pr-str (get-in imapstore-names->credentials [k :pass]))]
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
          (sort-by key (get sections type))))

(s/defn ^:private render :- String
  [sections                     :- Sections
   imapstore-names->credentials :- {Word IMAPCredentials}]
  (let [s (update-in sections [:imapstore]
                     #(replace-passcmd % imapstore-names->credentials))
        r (partial render-section s)]
    (string/join \newline (concat (:general s)
                                  [""]
                                  (r :imapstore)
                                  (r :maildirstore)
                                  (r :channel)))))

(s/defn parse :- Mbsyncrc
  [s :- String]
  (let [sections (parse-tokens (tokenize s))
        imapstore-names->credentials (map-credentials (:imapstore sections))
        channels->maildirstores (map-slave-maildirstores
                                  (:channel sections)
                                  (map-maildirstores (:maildirstore sections)))]
    (strict-map->Mbsyncrc
      {:text (render sections imapstore-names->credentials)
       :sections sections
       :channels (-> sections :channel keys set)
       :channels->maildirstores channels->maildirstores})))
