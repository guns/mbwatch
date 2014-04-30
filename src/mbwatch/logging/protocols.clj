(ns mbwatch.logging.protocols)

(defprotocol Loggable
  (log-level [this] "Returns this object's logging level")
  (log-item [this] "Returns a new LogItem object"))

(defprotocol ILogger
  (log [this ^LogItem log-item] "Handles a LogItem"))
