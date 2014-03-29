(ns mbwatch.console-logger-test
  (:require [clojure.test :refer [deftest is]]
            [mbwatch.console-logger :as c]
            [mbwatch.logging :refer [->log log]]
            [schema.core :refer [validate]])
  (:import (clojure.lang Keyword)
           (java.io StringWriter)
           (org.joda.time.format DateTimeFormat)))

(deftest test-sgr
  (is (validate {Keyword String} c/SGR)))

(deftest test-get-default-colors
  (is (= (count (c/get-default-colors)) 8)))

(defn logger-out [colors log-item]
  (let [s (StringWriter.)
        logger (c/->ConsoleLogger s colors (DateTimeFormat/forPattern "❤"))]
    (log logger log-item)
    (str s)))

(defn make-colors [color]
  (vec (take 8 (repeat color))))

(deftest test-ConsoleLogger
  (is (= "\033[31;48;5;100m[❤] Hello world.\033[0m\n"
         (logger-out (make-colors [:red :bg100]) (->log "Hello world."))
         (logger-out (make-colors [31 "48;5;100"]) (->log "Hello world."))
         (logger-out (make-colors "31;48;5;100") (->log "Hello world."))))
  (is (= "[❤] Hello world.\n"
         (logger-out nil (->log "Hello world.")))))
