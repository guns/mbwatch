(ns mbwatch.notification.search-test
  (:require [clojure.core.async :refer [chan close!]]
            [clojure.test :refer [is]]
            [mbwatch.events :refer [strict-map->MbsyncEventStop]]
            [mbwatch.logging.levels :refer [DEBUG]]
            [mbwatch.notification.search :refer [search-and-notify!]]
            [mbwatch.test.common :refer [chanv with-output]]
            [mbwatch.test.maildir :refer [with-emails]]
            [mbwatch.types :refer [strict-map->Maildirstore
                                   strict-map->NotifySpec]]
            [schema.test :refer [deftest]])
  (:import (javax.mail.internet MimeMessage MimeUtility)
           (mbwatch.types PatternWrapper)
           (org.joda.time DateTime)))

(def NOTIFY-SPEC
  (strict-map->NotifySpec
    {:strategy :all
     :blacklist {}
     :whitelist {}
     :patterns {}}))

(defn stop-event [timestamp]
  (strict-map->MbsyncEventStop
    {:id 0
     :mbchan "testing"
     :mboxes #{"INBOX"}
     :start (DateTime. timestamp)
     :stop (DateTime. timestamp)
     :level DEBUG
     :status 0
     :error nil
     :maildir (strict-map->Maildirstore
                {:inbox "test-resources/Maildirs/testing/INBOX"
                 :path "test-resources/Maildirs/testing/"
                 :flatten "."})}))

(defmacro with-search-results
  {:requires [#'is #'with-emails #'with-output search-and-notify! MimeMessage MimeUtility]}
  [[out-sym ids-sym] notify-spec & body]
  `(let [events# [(~stop-event 0)]
         log-chan# (~chan 0x1000)]
     (with-emails [[{:From "Alice <alice@example.com>"
                     :To "Bob <bob@example.com>"
                     :Date 0 :ID 0}
                    "Hello Bob."]
                   [{:From "Bob <bob@example.com>"
                     :To "Alice <alice@example.com>"
                     :Date 1 :ID 1}
                    "Hello Alice."]
                   [{:From (str (MimeUtility/encodeText "★ ❤ Carol ❤ ★") " <carol@example.com>")
                     :To "Bob <bob@example.com>"
                     :Date 2 :ID 2}
                    "Alice sent you a photo, click here!"]]
       (let [[o# e# _#] (with-output
                          (search-and-notify! events# ~notify-spec "cat" log-chan#)
                          (~close! log-chan#))
             ~out-sym o#
             ~ids-sym (-> (~chanv log-chan#)
                          peek
                          (get-in [:mbchan->mbox->messages "testing" "INBOX"])
                          (as-> v#
                            (mapv #(Long/parseLong
                                     (.getHeader ^MimeMessage % "ID" ""))
                                  v#))
                          set)]
         (is (= "" e#))
         ~@body))))

(deftest test-notify-spec
  (with-search-results [out ids]
    NOTIFY-SPEC
    (is (= ids #{0 1 2})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC :strategy :none)
    (is (= ids #{})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC :blacklist {"testing" #{}})
    (is (= ids #{})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC :blacklist {"testing" #{"INBOX"}})
    (is (= ids #{})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC :blacklist {"testing" #{"UNKNOWN"}})
    (is (= ids #{0 1 2})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC :strategy :match)
    (is (= ids #{})))
  (with-search-results [out ids]
    (assoc NOTIFY-SPEC
           :strategy :match
           :patterns {"from" #{(PatternWrapper. #"(?i)\balice@example\.com\b")}})
    (is (= ids #{0}))))
