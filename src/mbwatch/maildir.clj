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
  "Seq of new messages in maildir newer than given timestamp."
  [mdir-path :- String
   mtime     :- long]
  (->> (io/file mdir-path "new")
       .listFiles
       (filter (fn [^File f] (and (.isFile f) (> (.lastModified f) mtime))))
       (mapv message)))

(s/defn senders :- #{String}
  "Sorted set of senders in a sequence of MimeMessage objects."
  [messages :- [MimeMessage]]
  (->> messages
       (mapcat #(.getFrom ^MimeMessage %))
       (mapv #(MimeUtility/decodeText (str %)))
       (into (sorted-set))))
