(ns mbwatch.passwd
  (:require [clojure.string :as string]
            [schema.core :as s :refer [Int]]))

(s/defrecord Passwd
  [name   :- String
   passwd :- String
   uid    :- Int
   gid    :- Int
   gecos  :- String
   dir    :- String
   shell  :- String])

(s/defn parse-pwent :- Passwd
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
