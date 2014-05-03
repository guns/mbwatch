(ns mbwatch.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is testing]]
            [clojure.tools.cli :refer [get-default-options]]
            [mbwatch.config :refer [->Config config-options]]
            [mbwatch.config.mbsyncrc]
            [mbwatch.test.common :refer [with-tempfile]]
            [schema.core :refer [validate]]
            [schema.test :refer [deftest]])
  (:import (mbwatch.config.mbsyncrc Mbsyncrc)))

(deftest test-Config-ctor
  (let [{:keys [mbsyncrc idle sync notify blacklist notify-cmd conn-period
                sync-period conn-timeout imap-timeout]}
        (->Config {} (io/resource "mbsyncrc") (io/resource "mbwatchrc"))]
    (is (validate Mbsyncrc mbsyncrc))
    (is (= idle {"FOO-chan" #{"INBOX"} "BAR-chan" #{"INBOX"}}))
    (is (= sync {"FOO-chan" #{} "BAR-chan" #{}}))
    (is (= notify {"FOO-chan" #{"INBOX" "clojure"} "BAR-chan" #{}}))
    (is (= blacklist {"FOO-chan" #{"[Gmail].Spam"} "BAR-chan" #{"SPAM"}}))
    (is (= notify-cmd "notify - --audio=\"/home/guns/.sounds/new-message.mp3\""))
    (is (= conn-period 0))
    (is (= sync-period (* 10 60 1000)))
    (is (= conn-timeout 5000))
    (is (= imap-timeout 20000)))
  (testing "requires mbsyncrc"
    (is (thrown? RuntimeException
                 (->Config {} "/does/not/exist" (io/resource "mbwatchrc")))))
  (testing "ignores non-existant mbwatchrc"
    (is (= (-> (->Config {} (io/resource "mbsyncrc") "/does/not/exist")
               (dissoc :mbsyncrc))
           (get-default-options config-options))))
  (testing "merges idle and sync into notify when no notify entry exists"
    (with-tempfile tmp
      (spit tmp "idle = home:INBOX work:INBOX\nsync = home")
      (is (= (:notify (->Config {} (io/resource "mbsyncrc") tmp))
             {"home" #{} "work" #{"INBOX"}}))
      (spit tmp "idle = home:INBOX work:INBOX\nsync = home\nnotify = home")
      (is (= (:notify (->Config {} (io/resource "mbsyncrc") tmp))
             {"home" #{}}))))
  (testing "flattens notify-map and blacklist-map"
    (with-tempfile tmp
      (spit tmp "notify = FOO-chan:foo/bar\nblacklist = FOO-chan:foo/spam")
      (let [config (->Config {} (io/resource "mbsyncrc") tmp)]
        (is (= (:notify config) {"FOO-chan" #{"foo.bar"}}))
        (is (= (:blacklist config) {"FOO-chan" #{"foo.spam"}}))))))
