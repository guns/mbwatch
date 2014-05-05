(ns mbwatch.console-test
  (:require [clojure.test :refer [is]]
            [mbwatch.console :refer [->ConsoleLogger SGR catch-print
                                     get-default-colors print-console]]
            [mbwatch.logging.levels :refer [NOTICE]]
            [mbwatch.logging.protocols :refer [log log-item]]
            [mbwatch.test.common :refer [with-output]]
            [schema.core :refer [validate]]
            [schema.test :refer [deftest]])
  (:import (clojure.lang Keyword)
           (java.io StringWriter)
           (org.joda.time.format DateTimeFormat)))

(defn make-colors [color]
  (vec (take 8 (repeat color))))

(deftest test-sgr
  (is (validate {Keyword String} SGR)))

(deftest test-get-default-colors
  (is (= (count (get-default-colors 0)) 8)))

(deftest test-print-console
  (let [[out err _] (with-output
                      (print-console "foo")
                      (print-console :err "bar")
                      (print-console NOTICE :out "baz"))]
    (is (= out "\rfoo\n\r\033[32mbaz\033[0m\n"))
    (is (= err "\rbar\n"))))

(deftest test-catch-print
  (let [[o e v] (with-output (catch-print (assert false "TESTING")))]
    (is (= "" o))
    (is (re-find #"(?i)failed.*TESTING" e))
    (is (nil? v))))

(deftest test-ConsoleLogger
  (letfn [(logger-out [colors log-item]
            (let [s (StringWriter.)
                  logger (->ConsoleLogger s colors (DateTimeFormat/forPattern "❤"))]
              (log logger log-item)
              (str s)))]
    (is (= "\033[31;48;5;100m\r[❤] Hello world.\n\033[0m"
           (logger-out (make-colors [:red :bg100]) (log-item "Hello world."))
           (logger-out (make-colors [31 "48;5;100"]) (log-item "Hello world."))
           (logger-out (make-colors "31;48;5;100") (log-item "Hello world."))))
    (is (= "\r[❤] Hello world.\n"
           (logger-out nil (log-item "Hello world."))))))
