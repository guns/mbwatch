(ns mbwatch.maildir
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.set :refer [subset?]]
            [clojure.string :as string]
            [mbwatch.types :refer [MBMap Word]]
            [schema.core :as s :refer [maybe]])
  (:import (java.io File)
           (java.nio.file Files Path)
           (mbwatch.types Maildirstore)))

(s/defn ^:private maildir? :- Boolean
  [f :- File]
  (and (.isDirectory f)
       (subset? #{"cur" "new" "tmp"} (set (.list f)))))

(s/defn ^:private flatten-mbox :- String
  "Replace `/` characters in mbox with `flatten` parameter."
  [mbox    :- String
   flatten :- (maybe String)]
  (if flatten
    (string/join flatten (string/split mbox #"/"))
    mbox))

(s/defn flatten-mbmap :- MBMap
  "Replace `/` characters in mboxes with :flatten parameter."
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

(s/defn new-message-files :- [File]
  "Vector of new message files in maildir newer than given timestamp, sorted
   by most recent first."
  [mdir  :- Coercions
   mtime :- long]
  (with-open [ds (Files/newDirectoryStream (.toPath (io/file mdir "new")))]
    (->> ds
         (map #(.toFile ^Path %))
         (filter (fn [^File f]
                   (and (.isFile f)
                        (not (.isHidden f))
                        (> (.lastModified f) mtime))))
         (sort-by #(- (.lastModified ^File %))))))
