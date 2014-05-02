(ns mbwatch.keystore-test
  (:require [clojure.java.io :as io]
            [mbwatch.keystore :refer [write-certificate-keystore!]]
            [mbwatch.test.common :refer [with-tempfile]]
            [clojure.test :refer [is]]
            [schema.test :refer [deftest]])
  (:import (java.security KeyStore)
           (java.security.cert CertificateFactory)))

(deftest test-write-certificate-keystore!
  (with-tempfile tmp
    (write-certificate-keystore! tmp (io/resource "gmail.crt"))
    (with-open [file (io/input-stream tmp)
                crt (io/input-stream (io/resource "gmail.crt"))]
      (let [ks (KeyStore/getInstance (KeyStore/getDefaultType))]
        (.load ks file (char-array 0))
        (is (= (.getCertificate ks "mbwatch.keystore")
               (.generateCertificate (CertificateFactory/getInstance "X.509") crt)))))))
