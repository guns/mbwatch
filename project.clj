(defproject mbwatch "2.0.0-SNAPSHOT"
  :description "mbsync daemon"
  :main mbwatch.core
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0-beta2"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [org.clojure/tools.cli "0.3.1"]
                 [com.sun.mail/imap "1.5.1"]
                 [com.sun.mail/mailapi "1.5.1"]
                 [prismatic/schema "0.2.1"]
                 [com.stuartsierra/component "0.2.1"]
                 [joda-time/joda-time "2.3"]
                 [clojure-ini "0.0.2"]]
  :profiles {:dev {:jvm-opts ["-Djavax.net.ssl.trustStore=test-resources/gmail.ks"]
                   :source-paths ["dev"]
                   :resource-paths ["test-resources"]}})
