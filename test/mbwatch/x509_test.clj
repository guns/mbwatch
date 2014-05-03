(ns mbwatch.x509-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [mbwatch.x509 :refer [ssl-socket-factory]]
            [schema.test :refer [deftest]])
  (:import (java.net InetAddress InetSocketAddress Socket)
           (javax.net.ssl SSLSocket)))

(deftest test-ssl-socket-factory
  (let [sf (ssl-socket-factory (io/resource "example.com.crt"))
        host "example.com"
        port 443
        timeout 2000
        sockaddr (InetSocketAddress. (InetAddress/getByName host) port)]
    (with-open [s (Socket.)]
      (.connect s sockaddr timeout)
      (with-open [ss ^SSLSocket (.createSocket sf s host port true)]
        (.startHandshake ss)
        (is (.getSession ss))))))
