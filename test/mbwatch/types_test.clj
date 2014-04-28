(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
            [mbwatch.types :refer [schema-params]]
            [schema.test :refer [deftest]]))

(deftest test-defrecord
  (let [ns-sym `types-test#]
    (try
      (eval `(do (ns ~ns-sym ~'(:require [mbwatch.types :as t]))
                 ~'(t/defrecord Foo [bar :- Integer])))
      (is (= (mapv #(:private (meta (ns-resolve ns-sym %)))
                   ['->Foo 'map->Foo 'strict-map->Foo])
             [true true true]))
      (finally
        (remove-ns ns-sym)))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (schema-params '[foo :- Int bar baz :- #{Symbol}])
         (schema-params '[foo bar baz]))))
