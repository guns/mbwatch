(ns mbwatch.trie
  "A string trie, optimized for mbwatch commands."
  (:require [clojure.set :refer [union]]
            [clojure.string :as string]
            [immutable-int-map :refer [int-map]]
            [mbwatch.types :refer [TrieNode]]
            [schema.core :as s :refer [Any either maybe]]))

(def EMPTY-TRIE-NODE (int-map))

(s/defn add :- TrieNode
  [node  :- TrieNode
   input :- (either String [Character])
   value :- Any]
  (let [[ch & more] input
        n (int ch)
        node' (vary-meta (get node n EMPTY-TRIE-NODE)
                         #(merge-with union % {::values #{value}}))]
    (assoc node n (if more
                    (add node' more value)
                    node'))))

(s/defn add-command-aliases :- TrieNode
  [node  :- TrieNode
   input :- String
   value :- Any]
  (let [word-seqs (->> (string/split input #"\s+")
                       (mapv #(take-while seq (iterate drop-last (seq %)))))
        reduce-seqs (fn [node ws]
                      (reduce (fn [m s] (add m s value)) node ws))]
    ;; Commands are two words max
    (case (count word-seqs)
      1 (reduce-seqs node (first word-seqs))
      2 (let [[ws₁ ws₂] word-seqs
              ws (for [s₁ ws₁ s₂ ws₂] (concat s₁ [\space] s₂))]
          (reduce-seqs node ws)))))

(s/defn lookup :- (maybe #{Any})
  [node  :- TrieNode
   input :- String]
  (loop [node node [ch & more] input]
    (when ch
      (when-some [node (node ch)]
        (if more
          (recur node more)
          (::values (meta node)))))))
