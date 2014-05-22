(ns mbwatch.maildir-test
  (:require [clojure.test :refer [is]]
            [mbwatch.maildir :refer [flatten-mbmap get-all-mboxes get-mdir
                                     new-message-files]]
            [mbwatch.types :refer [strict-map->Maildirstore]]
            [schema.test :refer [deftest]])
  (:import (java.io File)))

(def ^String TEST-MDIR
  "test-resources/Maildirs/foo-mdir/INBOX")

(deftest test-flatten-mbmap
  (is (= (flatten-mbmap {"foo" #{"foo/bar/baz"}
                         "bar" #{"foo/bar/baz"}
                         "baz" #{"foo/bar/baz"}}
                        {"foo" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten "."})
                         "bar" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten "_"})
                         "baz" (strict-map->Maildirstore
                                 {:inbox "inbox" :path "path" :flatten nil})})
         {"foo" #{"foo.bar.baz"}
          "bar" #{"foo_bar_baz"}
          "baz" #{"foo/bar/baz"}})))

(deftest test-get-mdir
  (let [maildir (strict-map->Maildirstore
                  {:inbox "/home/user/Mail/INBOX"
                   :path "/home/user/Mail/gmail"
                   :flatten "."})]
    (is (= (get-mdir maildir "INBOX")
           "/home/user/Mail/INBOX"))
    (is (= (get-mdir maildir "clojure")
           "/home/user/Mail/gmail/clojure"))
    (is (= (get-mdir maildir "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail].Sent Mail"))
    (is (= (get-mdir (assoc maildir :flatten nil) "[Gmail]/Sent Mail")
           "/home/user/Mail/gmail/[Gmail]/Sent Mail"))))

(deftest test-get-all-mboxes
  (is (= (get-all-mboxes (strict-map->Maildirstore
                           {:path "test-resources/Maildirs/foo-mdir"
                            :inbox "test-resources/Maildirs/foo-mdir/INBOX"
                            :flatten nil}))
         #{"INBOX" "clojure"})))

(deftest test-new-message-files
  (let [fs (filterv (fn [^File f]
                      (and (.isFile f) (not (.isHidden f))))
                    (file-seq (File. TEST-MDIR)))]
    (is (= (count (new-message-files TEST-MDIR 0))
           (count fs)))
    (is (= (new-message-files TEST-MDIR 0)
           (sort-by #(- (.lastModified ^File %)) fs)))))
