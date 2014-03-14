(ns mbwatch.console-logger-test
  (:require [clojure.test :refer [deftest is]]
            [mbwatch.console-logger :as c]
            [mbwatch.logging]
            [schema.core :refer [validate]])
  (:import (clojure.lang Keyword)
           (java.io StringWriter)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)
           (org.joda.time.format DateTimeFormat)))

(deftest test-sgr
  (is (validate {Keyword String} c/sgr)))

(deftest test-sgr-join
  (is (= "" (c/sgr-join nil) (c/sgr-join [])))
  (is (= "31;48;5;100" (c/sgr-join [:red :bg100])))
  (is (= "31;48;5;100" (c/sgr-join [31 "48;5;100"]))))

(deftest test-ConsoleLogger
  (let [s (StringWriter.)
        dt (DateTime.)
        dt-str (.print (DateTimeFormat/forPattern "HH:mm:ss") dt)
        item (LogItem. 0 dt "Hello world.")]
    (.log (c/->ConsoleLogger s [[:red]]) item)
    (is (= (format "\033[31m[%s] Hello world.\033[0m\n" dt-str) (str s)))
    (.setLength (.getBuffer s) 0)
    (.log (c/->ConsoleLogger s nil) item)
    (is (= (format "[%s] Hello world.\n" dt-str) (str s)))))
