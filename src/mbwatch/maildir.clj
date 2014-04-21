(ns mbwatch.maildir
  (:require [clojure.java.io :as io]
            [schema.core :as s])
  (:import (java.io File FileInputStream)
           (javax.mail Session)
           (javax.mail.internet MimeMessage MimeUtility)))

(s/defn ^:private message :- MimeMessage
  [file :- File]
  (MimeMessage. (Session/getDefaultInstance (System/getProperties))
                (FileInputStream. file)))

(s/defn new-messages :- [MimeMessage]
  "Vector of new messages in maildir newer than given timestamp. Messages are
   sorted in reverse order by timestamp."
  [mdir-path :- String
   mtime     :- long]
  (->> (io/file mdir-path "new")
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
