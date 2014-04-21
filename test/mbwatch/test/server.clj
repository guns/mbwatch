(ns mbwatch.test.server
  (:import (java.net InetAddress InetSocketAddress ServerSocket Socket)
           (java.util.concurrent.atomic AtomicLong)))

(def ^InetAddress TEST-SERVER
  (InetAddress/getByAddress (byte-array [127 0xD0 0xC0 0xDE])))

(def ^:private ^AtomicLong server-id
  (AtomicLong. 0))

(defn next-server-port []
  (+ (rem (.getAndIncrement server-id) 0xfc00) 0x400))

(defn listen
  "Listen on TEST-SERVER:port until a client sends a single zero byte."
  [port]
  (with-open [ss (ServerSocket. port 0xff TEST-SERVER)]
    (loop []
      (let [n (try
                (with-open [s (.accept ss)
                            in (.getInputStream s)]
                  (.read in))
                (catch Throwable _
                  0xff))]
        (when-not (zero? n)
          (recur))))))

(defn poison-server
  "Send TEST-SERVER:port a single zero byte."
  [^long port]
  (try
    (with-open [s (Socket.)]
      (let [sockaddr (InetSocketAddress. TEST-SERVER port)]
        (.connect s sockaddr 1000)
        (with-open [out (.getOutputStream s)]
          (.write out 0x00))))
    (catch Throwable _)))

(defmacro with-server
  [[& host-port-syms] & body]
  (let [[host port & host-port-syms] host-port-syms]
    (if port
      `(let [~host ~(.getHostName TEST-SERVER) ~port (~next-server-port)
             f# (future (listen ~port))]
         (try
           (with-server ~host-port-syms
             ~@body)
           (finally
             (poison-server ~port)
             (future-cancel f#))))
      `(do ~@body))))
