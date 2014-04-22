(ns mbwatch.console-test
  (:require [clojure.test :refer [is]]
            [mbwatch.console :refer [->ConsoleLogger SGR get-default-colors]]
            [mbwatch.logging :refer [log log-item]]
            [schema.core :refer [validate]]
            [schema.test :refer [deftest]])
  (:import (clojure.lang Keyword)
           (java.io StringWriter)
           (org.joda.time.format DateTimeFormat)))

(deftest test-sgr
  (is (validate {Keyword String} SGR)))

(deftest test-get-default-colors
  (is (= (count (get-default-colors)) 8)))

(defn logger-out [colors log-item]
  (let [s (StringWriter.)
        logger (->ConsoleLogger s colors (DateTimeFormat/forPattern "❤"))]
    (log logger log-item)
    (str s)))

(defn make-colors [color]
  (vec (take 8 (repeat color))))

(deftest test-ConsoleLogger
  (is (= "\033[31;48;5;100m[❤] Hello world.\033[0m\n"
         (logger-out (make-colors [:red :bg100]) (log-item "Hello world."))
         (logger-out (make-colors [31 "48;5;100"]) (log-item "Hello world."))
         (logger-out (make-colors "31;48;5;100") (log-item "Hello world."))))
  (is (= "[❤] Hello world.\n"
         (logger-out nil (log-item "Hello world.")))))
