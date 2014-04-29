(ns mbwatch.maildir
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.set :refer [subset?]]
            [clojure.string :as string]
            [mbwatch.config.mbsyncrc :refer [Maildirstore]]
            [schema.core :as s])
  (:import (java.io File FileInputStream)
           (javax.mail Session)
           (javax.mail.internet MimeMessage MimeUtility)))

(s/defn ^:private message :- MimeMessage
  [file :- File]
  (MimeMessage. (Session/getDefaultInstance (System/getProperties))
                (FileInputStream. file)))

(s/defn ^:private maildir? :- Boolean
  [f :- File]
  (and (.isDirectory f)
       (subset? #{"cur" "new" "tmp"} (set (.list f)))))

(s/defn get-mdir :- File
  [maildir :- Maildirstore
   mbox    :- String]
  (let [{:keys [path inbox flatten]} maildir]
    (cond (= "INBOX" mbox) (io/file inbox)
          (nil? flatten) (io/file path mbox)
          :else (->> (string/split mbox #"/")
                     (string/join flatten)
                     (io/file path)))))

(s/defn get-all-mdirs :- #{File}
  [maildir :- Maildirstore]
  (let [{:keys [inbox path]} maildir
        inbox-dir (io/file inbox)
        mdirs (cond-> #{}
                (maildir? inbox-dir) (conj inbox-dir))]
    (->> (io/file path)
         .listFiles
         (filterv (fn [^File f] (and (.isDirectory f) (maildir? f))))
         (into mdirs))))

(s/defn new-messages :- [MimeMessage]
  "Vector of new messages in maildir newer than given timestamp. Messages are
   sorted in reverse order by timestamp."
  [mdir  :- Coercions
   mtime :- long]
  (->> (io/file mdir "new")
       .listFiles
       (filter (fn [^File f] (and (.isFile f) (> (.lastModified f) mtime))))
       (sort-by #(- (.lastModified ^File %)))
       (mapv message)))

(s/defn senders :- [String]
  "Vector of distinct senders in a sequence of MimeMessage objects."
  [messages :- [MimeMessage]]
  (->> messages
       (mapcat #(.getFrom ^MimeMessage %))
       distinct
       (mapv #(MimeUtility/decodeText (str %)))))
