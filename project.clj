(defproject mbwatch "2.0.0-SNAPSHOT"
  :description "mbsync daemon"
  :main mbwatch.core
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [org.clojure/tools.cli "0.3.1"]
                 [com.sun.mail/imap "1.5.1"]
                 [com.sun.mail/mailapi "1.5.1"]
                 [prismatic/schema "0.2.1"]]
  :profiles {:dev {:jvm-opts ["-Djavax.net.ssl.trustStore=test-resources/gmail.ks"]
                   :resource-paths ["test-resources"]}})
