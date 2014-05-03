(ns mbwatch.x509
  (:require [clojure.java.io :as io :refer [Coercions]]
            [schema.core :as s])
  (:import (java.security KeyStore)
           (java.security.cert CertificateFactory X509Certificate)
           (javax.net.ssl SSLContext SSLSocketFactory TrustManagerFactory)))

(s/defn ^:private make-certificates :- [X509Certificate]
  [x509-cert-file :- Coercions]
  (with-open [input (io/input-stream x509-cert-file)]
    (vec (.generateCertificates (CertificateFactory/getInstance "X.509") input))))

(s/defn ^:private make-keystore :- KeyStore
  [x509-certs :- [X509Certificate]]
  (let [ks (KeyStore/getInstance (KeyStore/getDefaultType))]
    (.load ks nil (char-array 0))
    (dotimes [i (count x509-certs)]
      (.setCertificateEntry ks (str i) (nth x509-certs i)))
    ks))

(s/defn ^:private make-trust-managers :- (Class/forName "[Ljavax.net.ssl.TrustManager;")
  [x509-cert-file :- Coercions]
  (let [tmf (TrustManagerFactory/getInstance
              (TrustManagerFactory/getDefaultAlgorithm))]
    (.init tmf (make-keystore (make-certificates x509-cert-file)))
    (.getTrustManagers tmf)))

(s/defn ssl-socket-factory :- SSLSocketFactory
  [x509-cert-file :- Coercions]
  (let [ctxt (SSLContext/getInstance "TLS")]
    (.init ctxt nil (make-trust-managers x509-cert-file) nil)
    (.getSocketFactory ctxt)))
