(ns mbwatch.test.maildir
  (:require [clojure.java.io :as io]
            [mbwatch.time :refer [dt->ms]])
  (:import (java.io File)
           (org.joda.time DateTime)
           (org.joda.time.format DateTimeFormat DateTimeFormatter)))

(def ^DateTimeFormatter RFC-822-DATEFORMAT
  (DateTimeFormat/forPattern "EEE, dd MMM yyyy HH:mm:ss Z"))

(def ^File TEST-MAILDIR
  (io/file "test-resources/Maildirs/testing"))

(defn clean-test-maildir! []
  (doseq [^File f (file-seq TEST-MAILDIR)
          :when (and (.isFile f) (not (.isHidden f)))]
    (.delete f)))

(defn make-email [[headers body]]
  (let [buf (reduce-kv
              (fn [^StringBuilder buf k v]
                (let [v (case k
                          :Date (.print RFC-822-DATEFORMAT ^DateTime (DateTime. v))
                          v)]
                  (.append buf (str (name k) ": " v "\n"))))
              (StringBuilder.) headers)]
    (.append ^StringBuilder buf (str "\n" body))
    (str buf)))

(defn write-emails! [& email-specs]
  (doseq [spec email-specs
          :let [e (make-email spec)
                f (str (dt->ms (DateTime. (:Date (first spec)))) "." (hash e) ":2")]]
    (spit (io/file TEST-MAILDIR "INBOX" "new" f) e)))

(defmacro with-emails
  [emails & body]
  `(try
     (apply ~write-emails! ~emails)
     ~@body
     (finally
       (~clean-test-maildir!))))
