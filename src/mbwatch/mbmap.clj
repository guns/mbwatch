(ns mbwatch.mbmap
  "Tools for working with MBMap structures: {String #{String}}"
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string]
            [mbwatch.types :refer [MBMap MBTuple]]
            [schema.core :as s :refer [pair]]))

(s/defn parse-mbargs :- MBMap
  [argv :- [String]]
  (reduce
    (fn [m arg]
      (let [[[_ chan bs]] (re-seq #"\A([^:]+)(?=:(.*))?" arg)]
        (assoc m chan (if (and bs (seq bs))
                        (into #{} (string/split bs #","))
                        #{}))))
    {} argv))

(s/defn join-mbentry :- String
  [mbchan :- String
   mboxes :- #{String}]
  (if (seq mboxes)
    (str mbchan \: (string/join \, mboxes))
    (str mbchan)))

(s/defn join-mbmap :- String
  [mbmap :- MBMap]
  (->> mbmap
       (mapv (fn [[mbchan mboxes]] (join-mbentry mbchan mboxes)))
       (string/join \space)))

(s/defn mbmap->mbtuples :- #{MBTuple}
  [mbmap :- MBMap]
  (reduce-kv
    (fn [v mbchan mboxes]
      (reduce
        (fn [v mbox]
          (conj v [mbchan mbox]))
        v mboxes))
    #{} mbmap))

(s/defn mbtuples->mbmap :- MBMap
  [mbtuples :- #{MBTuple}]
  (reduce
    (fn [m [mbchan mbox]]
      (update-in m [mbchan] #(conj (or % #{}) mbox)))
    {} mbtuples))

(s/defn ^:private mbmap-diff* :- (pair MBMap "removals"
                                       MBMap "additions")
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce
    (fn [[rem add] mbchan]
      (let [s₁ (or (m₁ mbchan) #{})
            s₂ (or (m₂ mbchan) #{})
            Δ- (difference s₁ s₂)
            Δ+ (difference s₂ s₁)]
        [(cond-> rem (seq Δ-) (assoc mbchan Δ-))
         (cond-> add (seq Δ+) (assoc mbchan Δ+))]))
    [{} {}] (distinct (mapcat keys [m₁ m₂]))))

(s/defn mbmap-diff :- (pair #{MBTuple} "removals"
                            #{MBTuple} "additions")
  [m₁ :- MBMap
   m₂ :- MBMap]
  (->> (mbmap-diff* m₁ m₂)
       (mapv mbmap->mbtuples)))

(s/defn mbmap-disj :- MBMap
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce-kv
    (fn [m mbchan mboxes]
      (let [m (update-in m [mbchan] difference mboxes)]
        (if (seq (m mbchan))
          m
          (dissoc m mbchan))))
    m₁ m₂))
