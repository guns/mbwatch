(ns mbwatch.maildir
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.set :refer [subset?]]
            [clojure.string :as string]
            [mbwatch.types :refer [MBMap Word]]
            [schema.core :as s :refer [maybe]])
  (:import (java.io File FileInputStream)
           (javax.mail Session)
           (javax.mail.internet MimeMessage MimeUtility)
           (mbwatch.types Maildirstore)))

(s/defn ^:private message :- MimeMessage
  [file :- File]
  (MimeMessage. (Session/getDefaultInstance (System/getProperties))
                (FileInputStream. file)))

(s/defn ^:private maildir? :- Boolean
  [f :- File]
  (and (.isDirectory f)
       (subset? #{"cur" "new" "tmp"} (set (.list f)))))

(s/defn flatten-mbox :- String
  [mbox    :- String
   flatten :- (maybe String)]
  (if flatten
    (string/join flatten (string/split mbox #"/"))
    mbox))

(s/defn flatten-mbmap :- MBMap
  [mbmap                :- MBMap
   mbchan->Maildirstore :- {Word Maildirstore}]
  (reduce-kv
    (fn [m mbchan mboxes]
      (let [fl (:flatten (mbchan->Maildirstore mbchan))
            bs (into #{} (mapv #(flatten-mbox % fl) mboxes))]
        (assoc m mbchan bs)))
    {} mbmap))

(s/defn get-mdir :- String
  [maildir :- Maildirstore
   mbox    :- String]
  (let [{:keys [path inbox flatten]} maildir]
    (if (= "INBOX" mbox)
      (str (io/file inbox))
      (str (io/file path (flatten-mbox mbox flatten))))))

(s/defn get-all-mboxes :- #{String}
  [maildir :- Maildirstore]
  (let [{:keys [inbox path]} maildir
        inbox-file (io/file inbox)
        mdirs (cond-> #{}
                (maildir? inbox-file) (conj (.getName inbox-file)))]
    (reduce
      (fn [s ^File f]
        (if (and (.isDirectory f) (maildir? f))
          (conj s (.getName f))
          s))
      mdirs (.listFiles (io/file path)))))

(s/defn new-messages :- [MimeMessage]
  "Vector of new messages in maildir newer than given timestamp. Messages are
   sorted in reverse order by timestamp."
  [mdir  :- Coercions
   mtime :- long]
  (->> (io/file mdir "new")
       .listFiles
       (filter (fn [^File f]
                 (and (.isFile f)
                      (not (.isHidden f))
                      (> (.lastModified f) mtime))))
       (sort-by #(- (.lastModified ^File %)))
       (mapv message)))

(s/defn senders :- [String]
  "Vector of distinct senders in a sequence of MimeMessage objects."
  [messages :- [MimeMessage]]
  (->> messages
       (mapcat #(.getFrom ^MimeMessage %))
       distinct
       (mapv #(MimeUtility/decodeText (str %)))))
