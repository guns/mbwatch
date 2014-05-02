(ns mbwatch.posix
  "Sundry POSIX-related functions."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [clojure.string :as string]
            [mbwatch.types :as t]
            [schema.core :as s :refer [Int]])
  (:import (java.io File)
           (java.nio.file Files StandardOpenOption)
           (java.nio.file.attribute PosixFilePermission)))

;; cf. /usr/include/pwd.h: struct passwd
(t/defrecord Passwd
  [name   :- String
   passwd :- String
   uid    :- Int
   gid    :- Int
   gecos  :- String
   dir    :- String
   shell  :- String])

(s/defn ^:private parse-pwent :- Passwd
  [line :- String]
  (-> (string/split line #"(?<!\\):" 7)
      (as-> ls (mapv #(string/replace % #"\\(.)" "$1") ls))
      (update-in [2] #(Integer/parseInt %))
      (update-in [3] #(Integer/parseInt %))
      ((fn [[a b c d e f g]] (Passwd. a b c d e f g)))))

(s/defn parse-passwd :- {String Passwd}
  "Returns a map of Passwd records keyed by username. If there are multiple
   entries for a name, the last entry wins."
  [s :- String]
  (reduce
    (fn [m line]
      (let [pw (parse-pwent line)]
        (assoc m (:name pw) pw)))
    {} (string/split-lines s)))

(s/defn expand-user-path :- String
  [passwd-map :- {String Passwd}
   path       :- String]
  (if (= (first path) \~)
    (if (= (second path) \/)
      (str (System/getProperty "user.home") (subs path 1))
      (let [i (.indexOf path "/")
            user (subs path 1 i)
            dir (get-in passwd-map [user :dir])]
        (if dir
          (str dir (subs path i))
          path)))
    path))

(s/defn mode->permset :- #{PosixFilePermission}
  [mode :- Int]
  (let [ps (PosixFilePermission/values)]
    (reduce
      (fn [s n]
        (if (pos? (bit-and mode (bit-shift-left 1 n)))
          (conj s (aget ps (- 8 n)))
          s))
      #{} (range 9))))

(s/defn create-file :- File
  "Create a new file with given permissions. If the file already exists,
   truncate it and reset its permissions to mode."
  [path :- Coercions
   mode :- Int]
  (let [f (io/file path)
        p (.toPath f)
        opts (into-array StandardOpenOption [StandardOpenOption/CREATE
                                             StandardOpenOption/WRITE])]
    (with-open [bc (Files/newByteChannel p opts)]
      (Files/setPosixFilePermissions p (mode->permset mode))
      (.truncate bc 0))
    f))

(s/defn create-dir :- File
  "Like create-file, but creates a directory tree. Only the leaf directory is
   created with the given file mode."
  [path :- Coercions
   mode :- Int]
  (let [d (io/file path)]
    (io/make-parents d)
    (.mkdir d)
    (Files/setPosixFilePermissions (.toPath d) (mode->permset mode))
    d))

(s/defn remove-dir :- Boolean
  "Recursively delete a directory tree."
  [path :- Coercions]
  (let [dir (io/file path)]
    (doseq [^File f (.listFiles dir)]
      (when (.isDirectory f)
        (remove-dir f))
      (.delete f))
    (.delete dir)))
