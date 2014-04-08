(ns mbwatch.command
  "Commands are broadcast to all command listeners, therefore the namespace of
   Opcodes is global (KISS)."
  (:require [mbwatch.logging :refer [->LogItem DEBUG Loggable]]
            [mbwatch.types :as t]
            [schema.core :as s :refer [Any defschema enum]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong command-id
  "A synchronized counter for Commands. There is no requirement to be either
   predictable or unpredictable, so this can be implemented as an incrementing
   global var."
  (AtomicLong. 0))

(defschema ^:private Opcode
  (enum :sync              ; Synchronize mailboxes
        :sync/term         ; Terminate any running mbsync processes
        :conn/trigger      ; Re-check connections
        :conn/set-period   ; Set connection watcher's period
        :notify/add        ; Add {mbchan [mbox]} to notify-map
        :notify/remove     ; Remove {mbchan [mbox]} from notify-map
        :notify/set        ; Set notify-map to {mbchan [mbox]}
        :timer/trigger     ; Trigger the timer's monitor
        :timer/set-period  ; Set cyclic timer's period
        :timer/set-request ; Set cyclic timer's sync request
        ))

(t/defrecord Command
  [opcode    :- Opcode
   payload   :- Any
   id        :- Long
   timestamp :- DateTime]

  Loggable

  (log-level [_] DEBUG)
  (log-item [this] (->LogItem this (str "Command: " opcode " " payload))))

(s/defn ->Command :- Command
  ([opcode]
   (->Command opcode nil))
  ([opcode  :- Opcode
    payload :- Any]
   (Command. opcode payload (.incrementAndGet command-id) (DateTime.))))
