(ns mbwatch.network
  (:require [mbwatch.types :refer [PortNumber]]
            [schema.core :as s :refer [Int maybe]])
  (:import (java.io IOException)
           (java.net InetAddress InetSocketAddress Socket
                     SocketTimeoutException)
           (java.util.concurrent ExecutionException)))

(s/defn lookup :- (maybe InetAddress)
  "Resolve host with timeout. DNS requests that do not return in timeout
   milliseconds return nil."
  [host    :- String
   timeout :- Int]
  (try
    (let [req (future (InetAddress/getByName host))]
      (or (deref req timeout nil)
          (do (future-cancel req) nil)))
    (catch ExecutionException _
      nil)))

(s/defn reachable? :- Boolean
  "Determine if a remote server is available at the given port. Timeout is
   in milliseconds, and is used as both the DNS resolution timeout and the
   socket connect timeout. Therefore, the maximum possible timeout is twice
   the given number. This won't usually be the case as a resolution failure is
   a fail-fast condition."
  [host    :- String
   port    :- PortNumber
   timeout :- Int]
  (try
    (let [addr (lookup host timeout)
          sockaddr (and addr (InetSocketAddress. ^InetAddress addr ^int port))]
      (if sockaddr
        (with-open [s (Socket.)]
          (.connect s sockaddr timeout)
          true)
        false))
    (catch IOException _ false)
    (catch SocketTimeoutException _ false)))
