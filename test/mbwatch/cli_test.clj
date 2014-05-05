(ns mbwatch.cli-test
  (:require [clojure.test :refer [is]]
            [mbwatch.cli :refer [parse-argv!]]
            [mbwatch.test.common :refer [with-output]]
            [schema.test :refer [deftest]]))

(deftest test-command-line-argument-parsing
  (let [[o e v] (with-output (parse-argv! ["--help"]))]
    (is (re-find #"(?s)Options:.*--help" o))
    (is (= "" e))
    (is (true? v)))
  (let [[o e v] (with-output (parse-argv! ["extra" "args"]))]
    (is (= "" o))
    (is (re-find #"(?i)WARNING" e))
    (is (false? v)))
  (let [[o e v] (with-output (parse-argv! ["-i" "home:INBOX"]))]
    (is (= "" o e))
    (is (= v {:idle {"home" #{"INBOX"}}}))))
