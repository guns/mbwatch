(ns mbwatch.connection-watcher-test
  (:require [clojure.test :refer [is testing]]
            [schema.test :refer [deftest]]))

(deftest test-private-behavior
  (let [merge-pending-syncs #'mbwatch.connection-watcher/merge-pending-syncs]
    (is (= (merge-pending-syncs {"a" {:status false :pending-syncs nil}}
                                {"a" ["INBOX"]})
           [{"a" {:status false :pending-syncs #{"INBOX"}}} {}]))
    (is (= (merge-pending-syncs {"a" {:status false :pending-syncs #{"INBOX"}}}
                                {"a" ["work"]})
           [{"a" {:status false :pending-syncs #{"INBOX" "work"}}} {}]))
    (is (= (merge-pending-syncs {"a" {:status false :pending-syncs ^:all-mboxes #{}}}
                                {"a" ["work"]})
           [{"a" {:status false :pending-syncs ^:all-mboxes #{}}} {}]))
    (is (= (merge-pending-syncs {"a" {:status true :pending-syncs #{"INBOX"}}}
                                {"a" ["work"]})
           [{"a" {:status true :pending-syncs #{"INBOX"}}}
            {"a" ["work"]}])))
  (let [update-connections-for-sync #'mbwatch.connection-watcher/update-connections-for-sync
        vfn (juxt identity (comp :mbwatch.connection-watcher/sync-req meta))
        up {:host "example.com" :port 80 :user "test" :pass "test"}
        down {:host "IDENT-PORT.example.com" :port 113 :user "test" :pass "test"}
        all-up {"a" up "b" up}
        all-down {"a" down "b" down}
        mixed {"a" up "b" down}]
    (is (= (vfn (update-connections-for-sync {"a" {:status true :pending-syncs nil}
                                              "b" {:status true :pending-syncs nil}}
                                             {"a" ["INBOX"]}
                                             all-down))
           [{"a" {:status false :pending-syncs #{"INBOX"}}
             "b" {:status true :pending-syncs nil}}
            {}]))
    (is (= (vfn (update-connections-for-sync {"a" {:status false :pending-syncs #{"INBOX"}}
                                              "b" {:status true :pending-syncs nil}}
                                             {"a" ["work"]}
                                             all-down))
           [{"a" {:status false :pending-syncs #{"INBOX" "work"}}
             "b" {:status true :pending-syncs nil}}
            {}]))
    (is (= (vfn (update-connections-for-sync {"a" {:status false :pending-syncs nil}
                                              "b" {:status false :pending-syncs nil}}
                                             {"a" ["INBOX"] "b" ["INBOX"]}
                                             all-up))
           [{"a" {:status true :pending-syncs #{"INBOX"}}
             "b" {:status true :pending-syncs #{"INBOX"}}}
            ;; The watch-fn is responsible for releasing these syncs
            {}]))
    (is (= (vfn (update-connections-for-sync {"a" {:status true :pending-syncs nil}
                                              "b" {:status true :pending-syncs nil}}
                                             {"a" ["INBOX"] "b" ["INBOX"]}
                                             mixed))
           [{"a" {:status true :pending-syncs nil}
             "b" {:status false :pending-syncs #{"INBOX"}}}
            {"a" ["INBOX"]}]))))
