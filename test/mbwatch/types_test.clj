(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
            [mbwatch.types :as t]
            [schema.test :as s]))

(s/deftest test-Passwd
  (is (= (t/parse-passwd "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")
         [#mbwatch.types.Passwd{:name "root" :passwd "x" :uid 0    :gid 0    :gecos ""   :dir "/root"      :shell "/bin/bash"}
          #mbwatch.types.Passwd{:name "guns" :passwd "x" :uid 1000 :gid 1000 :gecos ":)" :dir "/home/guns" :shell "/bin/bash"}])))

(s/deftest test-expand-user-path
  (let [passwd (t/parse-passwd "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")]
    (is (= "/root/mail" (t/expand-user-path passwd "~root/mail")))
    (is (= "/home/guns/mail" (t/expand-user-path passwd "~guns/mail")))
    (is (= "~test/mail" (t/expand-user-path passwd "~test/mail")))))
