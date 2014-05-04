(ns mbwatch.connection-watcher-test
  (:require [clojure.test :refer [is]]
            [schema.test :refer [deftest]]))

(deftest test-private-behavior
  (let [tfn #'mbwatch.connection-watcher/merge-pending-syncs]
    (is (= (tfn {"a" {:status false :pending-syncs nil}}
                {"a" #{"INBOX"}})
           [{"a" {:status false :pending-syncs #{"INBOX"}}} {}]))
    (is (= (tfn {"a" {:status false :pending-syncs #{"INBOX"}}}
                {"a" #{"work"}})
           [{"a" {:status false :pending-syncs #{"INBOX" "work"}}} {}]))
    (is (= (tfn {"a" {:status false :pending-syncs #{}}}
                {"a" #{"work"}})
           [{"a" {:status false :pending-syncs #{}}} {}]))
    (is (= (tfn {"a" {:status true :pending-syncs #{"INBOX"}}}
                {"a" #{"work"}})
           [{"a" {:status true :pending-syncs #{"INBOX"}}}
            {"a" #{"work"}}])))
  (let [tfn (fn [& args]
              (-> #'mbwatch.connection-watcher/update-connections-for-sync
                  (apply args)
                  ((juxt identity (comp :mbwatch.connection-watcher/sync-req meta)))))
        up {:host "example.com" :port 80 :user "test" :pass "test" :ssl? true :cert nil}
        down {:host "IDENT-PORT.example.com" :port 113 :user "test" :pass "test" :ssl? true :cert nil}
        all-up {"a" up "b" up}
        all-down {"a" down "b" down}
        mixed {"a" up "b" down}]
    (is (= (tfn {"a" {:status true :pending-syncs nil}
                 "b" {:status true :pending-syncs nil}}
                {"a" #{"INBOX"}}
                all-down 0)
           [{"a" {:status false :pending-syncs #{"INBOX"}}
             "b" {:status true :pending-syncs nil}}
            {}]))
    (is (= (tfn {"a" {:status false :pending-syncs #{"INBOX"}}
                 "b" {:status true :pending-syncs nil}}
                {"a" #{"work"}}
                all-down 0)
           [{"a" {:status false :pending-syncs #{"INBOX" "work"}}
             "b" {:status true :pending-syncs nil}}
            {}]))
    (is (= (tfn {"a" {:status false :pending-syncs nil}
                 "b" {:status false :pending-syncs nil}}
                {"a" #{"INBOX"} "b" #{"INBOX"}}
                all-up 2000)
           [{"a" {:status true :pending-syncs #{"INBOX"}}
             "b" {:status true :pending-syncs #{"INBOX"}}}
            ;; The watch-fn is responsible for releasing these syncs
            {}]))
    (is (= (tfn {"a" {:status true :pending-syncs nil}
                 "b" {:status true :pending-syncs nil}}
                {"a" #{"INBOX"} "b" #{"INBOX"}}
                mixed 2000)
           [{"a" {:status true :pending-syncs nil}
             "b" {:status false :pending-syncs #{"INBOX"}}}
            {"a" #{"INBOX"}}]))))
