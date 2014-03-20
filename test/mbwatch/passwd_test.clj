(ns mbwatch.passwd-test
  (:require [clojure.test :refer [is]]
            [mbwatch.passwd :as t]
            [schema.test :as s]))

(def PASSWD-BUF
  "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")

(s/deftest test-parse-passwd
  (is (= (t/parse-passwd PASSWD-BUF)
         {"root" #mbwatch.passwd.Passwd{:name "root" :passwd "x" :uid 0    :gid 0    :gecos ""   :dir "/root"      :shell "/bin/bash"}
          "guns" #mbwatch.passwd.Passwd{:name "guns" :passwd "x" :uid 1000 :gid 1000 :gecos ":)" :dir "/home/guns" :shell "/bin/bash"}})))

(s/deftest test-expand-user-path
  (let [passwd (t/parse-passwd PASSWD-BUF)]
    (is (= "/root/mail"      (t/expand-user-path passwd "~root/mail")))
    (is (= "/home/guns/mail" (t/expand-user-path passwd "~guns/mail")))
    (is (= "~bofh/mail"      (t/expand-user-path passwd "~bofh/mail")))))
