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
  (->> [[:help          VOID      "This help menu"]
        [:reload        VOID      "Reload configuration"]
        [:Restart       VOID      "Restart application"]
        [:quit          VOID      "Quit application"]

        [:idle/add      MBMap     "Add to watched mboxes"]
        [:idle/remove   MBMap     "Remove from watched mboxes"]
        [:idle/set      MBMap     "Set watched mboxes"]
        [:idle/Restart  VOID      "Restart IMAP connections"]

        [:sync          MBMap     "Synchronize given mailboxes"]
        [:sync/add      MBMap     "Add to periodic sync request"]
        [:sync/remove   MBMap     "Remove from periodic sync request"]
        [:sync/set      MBMap     "Set periodic sync request"]
        [:sync/kill     VOID      "Terminate running mbsync processes"]
        [:sync/period   Int       "Set sync period"]
        [:sync/trigger  VOID      "Trigger periodic sync"]

        [:conn/remove   #{String} "Remove from registered connections"]
        [:conn/period   Int       "Set connection check period"]
        [:conn/trigger  VOID      "Re-check connections"]

        [:notify/add    MBMap     "Add to notification mboxes"]
        [:notify/remove MBMap     "Remove from notification mboxes"]
        [:notify/set    MBMap     "Set notification mboxes"]]
       (mapcat (fn [[kw schema help]]
                 [kw (with-meta schema {:help help})]))
       (apply hash-map)))

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
