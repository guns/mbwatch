(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
            [schema.test :refer [deftest]]))

(deftest test-defrecord
  (let [ns-sym `types-test#]
    (try
      (eval `(do (ns ~ns-sym ~'(:require [mbwatch.types :as t]))
                 ~'(t/defrecord Foo [bar :- Integer])))
      (is (-> ns-sym (ns-resolve '->Foo) meta :private))
      (is (-> ns-sym (ns-resolve 'map->Foo) meta :private))
      (is (-> ns-sym (ns-resolve 'strict-map->Foo) meta :private))
      (finally
        (remove-ns ns-sym)))))
