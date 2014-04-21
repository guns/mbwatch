(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
            [mbwatch.types :refer [schema-params]]
            [schema.test :refer [deftest]]))

(deftest test-defrecord
  (let [ns-sym `types-test#]
    (try
      (eval `(do (ns ~ns-sym ~'(:require [mbwatch.types :as t]))
                 ~'(t/defrecord ^:private Foo [bar :- Integer])))
      (is (nil? (ns-resolve ns-sym '->Foo)))
      (is (nil? (ns-resolve ns-sym 'map->Foo)))
      (is (:private (meta (ns-resolve ns-sym 'strict-map->Foo))))
      (finally
        (remove-ns ns-sym)))))

(deftest test-schema-params
  (is (= '[foo bar baz]
         (schema-params '[foo :- Int bar :- String baz :- #{Symbol}])
         (schema-params '[foo :- Int bar baz :- #{Symbol}])
         (schema-params '[foo bar baz]))))
