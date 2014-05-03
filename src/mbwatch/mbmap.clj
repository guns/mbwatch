(ns mbwatch.mbmap
  "Tools for working with MBMap structures: {String #{String}}"
  (:require [clj-shellwords.core :refer [shell-split]]
            [clojure.set :refer [difference intersection]]
            [clojure.string :as string]
            [mbwatch.types :refer [MBMap MBMap+ MBTuple]]
            [schema.core :as s :refer [maybe pair]])
  (:import (clojure.lang IPersistentSet)))

(s/defn parse-mbargs :- MBMap
  [argv :- [String]]
  (reduce
    (fn [m arg]
      (let [[[_ chan bs]] (re-seq #"\A([^:]+)(?=:(.*))?" arg)]
        (assoc m chan (if (and bs (seq bs))
                        (into #{} (string/split bs #","))
                        #{}))))
    {} argv))

(s/defn parse-mbline :- MBMap
  [s :- String]
  (parse-mbargs (shell-split s)))

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

(s/defn ^:private diff* :- (maybe IPersistentSet)
  "Like clojure.set/difference, except that an empty set is coerced to nil."
  [s₁ :- IPersistentSet
   s₂ :- IPersistentSet]
  (let [Δ (difference s₁ s₂)]
    (when (seq Δ) Δ)))

(s/defn mbmap-diff+ :- (pair MBMap+ "removals"
                             MBMap+ "additions")
  "Diff of two MBMaps, where an empty set is equivalent to nil."
  [m₁ :- MBMap+
   m₂ :- MBMap+]
  (reduce
    (fn [[rem add] mbchan]
      (let [s₁ (or (m₁ mbchan) #{})
            s₂ (or (m₂ mbchan) #{})
            Δ- (difference s₁ s₂)
            Δ+ (difference s₂ s₁)]
        [(cond-> rem (seq Δ-) (assoc mbchan Δ-))
         (cond-> add (seq Δ+) (assoc mbchan Δ+))]))
    [{} {}] (distinct (mapcat keys [m₁ m₂]))))

(s/defn mbmap-diff :- (pair MBMap "removals"
                            MBMap "additions")
  "Diff of two MBMaps, where an empty set is equivalent to the universal set."
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce
    (fn [[rem add] mbchan]
      (let [s₁ (m₁ mbchan)
            s₂ (m₂ mbchan)
            [Δ- Δ+] (cond (nil? s₁) [nil s₂]
                          (nil? s₂) [s₁ nil]
                          (empty? s₁) [nil nil]
                          (empty? s₂) [nil s₂]
                          :else [(diff* s₁ s₂)
                                 (diff* s₂ s₁)])]
        [(cond-> rem (some? Δ-) (assoc mbchan Δ-))
         (cond-> add (some? Δ+) (assoc mbchan Δ+))]))
    [{} {}] (distinct (mapcat keys [m₁ m₂]))))

(s/defn mbmap-intersection :- MBMap
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce
    (fn [m k]
      (if (and (contains? m₁ k) (contains? m₂ k))
        (assoc m k (intersection (m₁ k) (m₂ k)))
        m))
    {} (mapcat keys [m₁ m₂])))

(s/defn ^:private mbmap-disj* :- MBMap
  [mbmap  :- MBMap
   mbchan :- String
   mboxes :- #{String}]
  (let [mboxes' (difference (mbmap mbchan) mboxes)]
    (if (seq mboxes')
      (assoc mbmap mbchan mboxes')
      (dissoc mbmap mbchan))))

(s/defn mbmap-disj :- MBMap
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce-kv
    (fn [m mbchan mboxes₁]
      (let [mboxes₀ (m mbchan)]
        (cond (empty? mboxes₁) (dissoc m mbchan)
              (empty? mboxes₀) m
              :else (mbmap-disj* m mbchan mboxes₁))))
    m₁ m₂))

(s/defn mbmap-merge+ :- MBMap+
  "Like (merge-with union m₁ m₂), except that missing values from m₁ are
   always substituted with empty sets."
  [m₁ :- MBMap+
   m₂ :- MBMap+]
  (merge-with (fn [s₁ s₂] (into (or s₁ #{}) s₂)) m₁ m₂))

(s/defn mbmap-merge :- MBMap
  "Merge two MBMaps, treating the empty set as universal.
   i.e. (conj #{} :any) -> #{}"
  [m₁ :- MBMap
   m₂ :- MBMap]
  (reduce-kv
    (fn [m mbchan mboxes₁]
      (let [mboxes₀ (m mbchan)]
        (cond (= mboxes₀ #{}) m
              (seq mboxes₁) (assoc m mbchan (into (or mboxes₀ #{}) mboxes₁))
              :else (assoc m mbchan #{}))))
    m₁ m₂))
