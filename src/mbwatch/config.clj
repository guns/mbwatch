(ns mbwatch.config
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [mbwatch.util :refer [chomp dequote]]
            [schema.core :as s :refer [both defschema either enum eq one
                                       optional-key pair pred]]))

(defschema Word
  (pred #(and (string? %)
              (seq %)
              (not (re-find #"\s" %)))))

(defschema LowerCaseWord
  (both Word (pred #(not (re-find #"\p{Lu}" %)))))

(defschema FilteredLine
  (pred #(and (string? %)
              (not (re-seq #"\n|\A\s*#|\A\s*\z|\A\s|\s\z" %)))
        "single non-comment line with no surrounding whitespace"))

(defschema Entry
  (pair LowerCaseWord "name"
        FilteredLine "value"))

(defschema Token
  (either
    [(one (enum :imapstore :maildirstore :channel) "section type")
     (one Word "section name")
     (one [Entry] "config key-value entries")]

    [(one (eq :general) "section type")
     (one (eq nil) "nil")
     (one [FilteredLine] "general config lines")]))

(defschema Sections
  {(optional-key :general)      [FilteredLine]
   (optional-key :imapstore)    {Word {LowerCaseWord FilteredLine}}
   (optional-key :maildirstore) {Word {LowerCaseWord FilteredLine}}
   (optional-key :channel)      {Word {LowerCaseWord FilteredLine}}})

(defschema PortNumber
  (pred #(and (integer? %) (< 0 % 0x1000))))

(defschema IMAPCredentials
  {:host String
   :port PortNumber
   :user String
   :pass String})

(s/defrecord Config
  [sections    :- Sections
   credentials :- {Word IMAPCredentials}])

(def ^:const default-path
  "Default path of mbsyncrc."
  (str (System/getProperty "user.home") \/ ".mbsyncrc"))

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
  [imapstores :- {Word {LowerCaseWord FilteredLine}}]
  (reduce-kv
    (fn [m store-name imap]
      (assoc m store-name (parse-credentials imap)))
    {} imapstores))

(s/defn parse :- Config
  [s :- String]
  (let [rc (parse-tokens (tokenize s))]
    (strict-map->Config
      {:sections rc
       :credentials (map-credentials (:imapstore rc))})))

(s/defn render :- String
  "Render an mbsyncrc map into a string."
  [rc :- Mbsyncrc]
  (letfn [(to-lines [kw]
            (mapcat (fn [[k kvs]]
                      (into [(string/join \space [(name kw) k])]
                            (conj (mapv #(string/join \space %) kvs) "")))
                    (get rc kw)))]
    (string/join \newline (concat (:general rc)
                                  [""]
                                  (to-lines :imapstore)
                                  (to-lines :maildirstore)
                                  (to-lines :channel)))))
