(ns mbwatch.command
  "Mbwatch components are configured and controlled via Commands."
  (:require [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.types :as t :refer [MBMap VOID]]
            [schema.core :as s :refer [Any Int both defschema enum pred
                                       validate]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong command-id
  "A synchronized counter for Commands. There is no requirement to be either
   predictable or unpredictable, so this can be implemented as an incrementing
   global var."
  (AtomicLong. 0))

(def ^:private OPCODE->PAYLOAD
  {:app/reload       VOID      ; Reload configuration
   :app/restart      VOID      ; Restart application
   :app/stop         VOID      ; Stop application

   :timer/trigger    VOID      ; Trigger the sync timer
   :timer/set-period Int       ; Set sync timer's period
   :timer/add        MBMap     ; Add to sync timer request
   :timer/remove     MBMap     ; Remove from sync timer request
   :timer/set        MBMap     ; Set sync timer request

   :idle/restart     VOID      ; Restart IMAP connections
   :idle/add         MBMap     ; Add to watched mboxes
   :idle/remove      MBMap     ; Remove from watched mboxes
   :idle/set         MBMap     ; Set watched mboxes

   :conn/trigger     VOID      ; Re-check connections
   :conn/set-period  Int       ; Set connection watcher's period
   :conn/remove      #{String} ; Remove from watched connections

   :sync             MBMap     ; Synchronize mailboxes
   :sync/term        VOID      ; Terminate any running mbsync processes

   :notify/add       MBMap     ; Add to notification mboxes
   :notify/remove    MBMap     ; Remove from notification mboxes
   :notify/set       MBMap     ; Set notification mboxes
   })

(defschema ^:private Opcode
  (apply enum (keys OPCODE->PAYLOAD)))

(t/defrecord ^:private Command
  [opcode    :- Opcode
   payload   :- Any
   id        :- long
   timestamp :- DateTime]

  Loggable

  (log-level [_] DEBUG)
  (log-item [this] (LogItem. DEBUG timestamp (str "Command: " opcode " " payload))))

(defschema ^:private CommandSchema
  (both Command
        (pred #(do (validate (OPCODE->PAYLOAD (:opcode %))
                             (:payload %))
                   true)
              "Valid command payload for opcode")))

(s/defn ->Command :- CommandSchema
  ([opcode]
   (->Command opcode nil))
  ([opcode  :- Opcode
    payload :- Any]
   (Command. opcode payload (.getAndIncrement command-id) (DateTime.))))
