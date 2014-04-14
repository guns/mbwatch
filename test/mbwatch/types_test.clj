(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
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
