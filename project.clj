(defproject mbwatch "2.0.0-SNAPSHOT"
  :description "mbsync daemon"
  :main mbwatch.core
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [org.clojure/tools.cli "0.3.2-SNAPSHOT"]
                 [com.sun.mail/imap "1.5.2"]
                 [com.sun.mail/mailapi "1.5.2"]
                 [prismatic/schema "0.2.2"]
                 [com.stuartsierra/component "0.2.1"]
                 [joda-time/joda-time "2.3"]
                 [immutable-int-map "0.1.0"]
                 [clj-shellwords "1.0.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.5.8"]]
                   :plugins [[jonase/eastwood "RELEASE"]]
                   :source-paths ["dev"]
                   :resource-paths ["test-resources"]
                   :eastwood {:exclude-linters [:redefd-vars]}
                   :jvm-opts ~(when (= (System/getenv "DEBUG") "1")
                                ["-Dmail.debug=true"
                                 "-Djava.security.debug=certpath"
                                 "-Djavax.net.debug=all"])}
             :build {:aot [mbwatch.core]
                     :source-paths ["dev"]
                     :target-path "target/build"
                     :plugins [[lein-bin "0.3.4"]]
                     :jvm-opts ~(when-let [jvm-opts (System/getenv "MBWATCH_JVM_OPTS")]
                                  (clojure.string/split jvm-opts #"\s+"))
                     :bin {:name "mbwatch-daemon"
                           :bootclasspath true}}}
  :aliases {"BUILD" ["with-profile" "build" "do" "bin,", "run" "-m" "build-hook"]})
