(ns mbwatch.message
  "MimeMessage convenience functions."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [schema.core :as s])
  (:import (java.io FileInputStream)
           (javax.mail Session)
           (javax.mail.internet MimeMessage MimeUtility)))

(s/defn message :- MimeMessage
  [file :- Coercions]
  (with-open [fis (FileInputStream. (io/file file))]
    (MimeMessage. (Session/getDefaultInstance (System/getProperties)) fis)))

(s/defn senders :- [String]
  [msg :- MimeMessage]
  (mapv #(MimeUtility/decodeText (str %)) (.getFrom msg)))

(s/defn subject :- String
  [msg :- MimeMessage]
  (MimeUtility/decodeText (.getSubject msg)))

(s/defn headers :- [String]
  [msg  :- MimeMessage
   name :- String]
  (mapv #(MimeUtility/decodeText %) (.getHeader msg name)))
