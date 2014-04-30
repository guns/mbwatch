(ns mbwatch.cli
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [mbwatch.config :refer [config-options]]
            [schema.core :as s :refer [Any either]])
  (:import (clojure.lang Keyword)))

(s/defn ^:private usage :- String
  [options-summary :- String]
  (format (slurp (io/resource "usage.txt")) options-summary))

(s/defn ^:private error-msg :- String
  [errors :- [String]]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(s/defn parse-argv! :- (either Boolean {Keyword Any})
  "Parses command line arguments, returning an options map for only the
   supplied options. Prints a usage banner and returns true on `--help`. If
   there are errors, they are printed to stderr and false is returned."
  [argv :- [String]]
  (let [opts (conj (config-options) ["-h" "--help"])
        {:keys [options arguments errors summary]} (parse-opts argv opts :no-defaults true)]
    (cond
      (or (:help options)
          (seq arguments)) (do (.println System/out (usage summary))
                               true)
      errors (do (.println System/err (error-msg errors))
                 false)
      :else options)))
