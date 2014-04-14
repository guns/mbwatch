(ns mbwatch.passwd-test
  (:require [clojure.test :refer [is]]
            [mbwatch.passwd :refer [expand-user-path parse-passwd]]
            [schema.test :refer [deftest]]))

(def PASSWD-BUF
  "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")

(deftest test-parse-passwd
  (is (= (parse-passwd PASSWD-BUF)
         {"root" #mbwatch.passwd.Passwd{:name "root" :passwd "x" :uid 0    :gid 0    :gecos ""   :dir "/root"      :shell "/bin/bash"}
          "guns" #mbwatch.passwd.Passwd{:name "guns" :passwd "x" :uid 1000 :gid 1000 :gecos ":)" :dir "/home/guns" :shell "/bin/bash"}})))

(deftest test-expand-user-path
  (let [passwd (parse-passwd PASSWD-BUF)]
    (is (= "/root/mail"      (expand-user-path passwd "~root/mail")))
    (is (= "/home/guns/mail" (expand-user-path passwd "~guns/mail")))
    (is (= "~bofh/mail"      (expand-user-path passwd "~bofh/mail")))))
