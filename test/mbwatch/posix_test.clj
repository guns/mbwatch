(ns mbwatch.posix-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is testing]]
            [mbwatch.posix :refer [create-dir create-file expand-user-path
                                   mode->permset parse-passwd remove-dir]]
            [schema.test :refer [deftest]])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file.attribute PosixFilePermission)))

(def PASSWD-BUF
  "root:x:0:0::/root:/bin/bash\nguns:x:1000:1000:\\:):/home/guns:/bin/bash")

(deftest test-parse-passwd
  (is (= (parse-passwd PASSWD-BUF)
         {"root" #mbwatch.posix.Passwd{:name "root" :passwd "x" :uid 0    :gid 0    :gecos ""   :dir "/root"      :shell "/bin/bash"}
          "guns" #mbwatch.posix.Passwd{:name "guns" :passwd "x" :uid 1000 :gid 1000 :gecos ":)" :dir "/home/guns" :shell "/bin/bash"}})))

(deftest test-expand-user-path
  (let [passwd (parse-passwd PASSWD-BUF)]
    (is (= "/root/mail"      (expand-user-path passwd "~root/mail")))
    (is (= "/home/guns/mail" (expand-user-path passwd "~guns/mail")))
    (is (= "~bofh/mail"      (expand-user-path passwd "~bofh/mail")))))

(deftest test-file-modes
  (is (= (mode->permset 0412)
         #{PosixFilePermission/OWNER_READ
           PosixFilePermission/GROUP_EXECUTE
           PosixFilePermission/OTHERS_WRITE})))

(deftest test-create-file
  (let [f (io/file "/tmp" (str `test-file-creation#))]
    (try
      (is (not (.exists f)))
      (create-file f 0700)
      (is (.exists f))
      (is (= (Files/getPosixFilePermissions (.toPath f) (make-array LinkOption 0))
             #{PosixFilePermission/OWNER_READ
               PosixFilePermission/OWNER_WRITE
               PosixFilePermission/OWNER_EXECUTE}))
      (create-file f 0444)
      (is (= (Files/getPosixFilePermissions (.toPath f) (make-array LinkOption 0))
             #{PosixFilePermission/OWNER_READ
               PosixFilePermission/GROUP_READ
               PosixFilePermission/OTHERS_READ}))
      (finally
        (.delete f)))))

(deftest test-create-dir
  (let [d₁ (io/file "/tmp" (str `test-dir-creation#) (str `test-dir-creation#))
        d₀ (.getParentFile d₁)]
    (try
      (is (not (.exists d₀)))
      (is (not (.exists d₁)))
      (create-dir d₁ 0700)
      (is (.exists d₀))
      (is (.exists d₁))
      (is (= (Files/getPosixFilePermissions (.toPath d₁) (make-array LinkOption 0))
             #{PosixFilePermission/OWNER_READ
               PosixFilePermission/OWNER_WRITE
               PosixFilePermission/OWNER_EXECUTE}))
      (create-dir d₁ 0750)
      (is (= (Files/getPosixFilePermissions (.toPath d₁) (make-array LinkOption 0))
             #{PosixFilePermission/OWNER_READ
               PosixFilePermission/OWNER_WRITE
               PosixFilePermission/OWNER_EXECUTE
               PosixFilePermission/GROUP_READ
               PosixFilePermission/GROUP_EXECUTE}))
      (finally
        (.delete d₁)
        (.delete d₀)))))

(deftest test-remove-dir
  (let [dir (create-dir (io/file "/tmp" (str `test-dir-creation#) (str `test-dir-creation#)) 0700)
        root (.getParentFile dir)]
    (create-file (io/file dir "testfile") 0600)
    (remove-dir root)
    (is (not (.exists root)))))
