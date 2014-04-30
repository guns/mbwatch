(defproject mbwatch "2.0.0-SNAPSHOT"
  :description "mbsync daemon"
  :main mbwatch.core
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.301.0-deb34a-alpha"]
                 [org.clojure/tools.cli "0.3.2-SNAPSHOT"]
                 [com.sun.mail/imap "1.5.1"]
                 [com.sun.mail/mailapi "1.5.1"]
                 [prismatic/schema "0.2.1"]
                 [com.stuartsierra/component "0.2.1"]
                 [joda-time/joda-time "2.3"]
                 [immutable-int-map "0.1.0"]
                 [clj-shellwords "1.0.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.5.7"]]
                   :plugins [[jonase/eastwood "RELEASE"]]
                   :jvm-opts ["-Djavax.net.ssl.trustStore=test-resources/gmail.ks"]
                   :source-paths ["dev"]
                   :resource-paths ["test-resources"]
                   :eastwood {:exclude-linters [:redefd-vars]}}
             :build {:aot [mbwatch.core]
                     :target-path "target/build"
                     :plugins [[lein-bin "0.3.4"]]
                     :jvm-opts ~(if-let [jvm-opts (System/getenv "MBWATCH_JVM_OPTS")]
                                  (clojure.string/split jvm-opts #"\s+")
                                  [])
                     :bin {:name "mbwatch"
                           :bootclasspath true}}}
  :aliases {"BUILD" ["with-profile" "build" "bin"]})
