(ns mbwatch.trie-test
  (:require [clojure.test :refer [is]]
            [mbwatch.trie :refer [EMPTY-TRIE-NODE add add-command-aliases
                                  lookup]]
            [schema.test :refer [deftest]]))

(deftest test-add
  (is (= (-> EMPTY-TRIE-NODE
             (add "abc" :abc)
             (add "abd" :abd))
         {#=(int \a) {#=(int \b) {#=(int \c) {}
                                  #=(int \d) {}}}})))

(deftest test-add-command-aliases
  (is (= (-> EMPTY-TRIE-NODE
             (add-command-aliases "abc def" :foo))
         {97 {32 {100 {101 {102 {}}}},
              98 {32 {100 {101 {102 {}}}},
                  99 {32 {100 {101 {102 {}}}}}}}})))

(deftest test-lookup
  (let [t (-> EMPTY-TRIE-NODE
              (add "abc" :abc)
              (add "abd" :abd))]

    (is (= (lookup t "a") #{:abc :abd}))
    (is (= (lookup t "abc") #{:abc}))
    (is (= (lookup t "d") nil))))
