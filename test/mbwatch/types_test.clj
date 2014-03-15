(ns mbwatch.types-test
  (:require [clojure.test :refer [is]]
            [mbwatch.types :as t]
            [schema.test :as s]))

(s/deftest test-Passwd
  (is (= (t/parse-passwd "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")
         [#mbwatch.types.Passwd{:name "root" :passwd "x" :uid 0    :gid 0    :gecos ""   :dir "/root"      :shell "/bin/bash"}
          #mbwatch.types.Passwd{:name "guns" :passwd "x" :uid 1000 :gid 1000 :gecos ":)" :dir "/home/guns" :shell "/bin/bash"}])))
