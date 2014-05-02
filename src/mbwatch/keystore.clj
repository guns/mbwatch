(ns mbwatch.keystore
  "Generates KeyStore objects from base64 encoded X.509 certificates."
  (:require [clojure.java.io :as io :refer [Coercions]]
            [mbwatch.posix :refer [create-file]]
            [mbwatch.types :refer [VOID]]
            [schema.core :as s])
  (:import (java.io OutputStream)
           (java.security KeyStore)
           (java.security.cert Certificate CertificateFactory)))

(s/defn ^:private ->Certificate :- Certificate
  [x509-cert-path :- Coercions]
  (with-open [crt (io/input-stream x509-cert-path)]
    (.generateCertificate (CertificateFactory/getInstance "X.509") crt)))

(s/defn ^:private ->KeyStore :- KeyStore
  [cert :- Certificate]
  (doto (KeyStore/getInstance (KeyStore/getDefaultType))
    (.load nil (char-array 0))
    (.setCertificateEntry "mbwatch.keystore" cert)))

(s/defn ^:private write-keystore! :- VOID
  [os :- OutputStream
   ks :- KeyStore]
  (.store ks os (char-array 0)))

(s/defn write-certificate-keystore! :- VOID
  "Write a base64 encoded X.509 certificate file to a serialized KeyStore with
   POSIX 0600 permissions. No password is set on the KeyStore."
  [output-ks-path  :- Coercions
   input-cert-path :- Coercions]
  (with-open [f (io/output-stream (create-file output-ks-path 0600))]
    (->> input-cert-path
         ->Certificate
         ->KeyStore
         (write-keystore! f))))
