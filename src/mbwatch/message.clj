(ns mbwatch.message
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
