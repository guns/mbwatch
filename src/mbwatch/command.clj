(ns mbwatch.command
  "Mbwatch components are configured and controlled via Commands."
  (:require [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.types :as t :refer [MBMap VOID]]
            [schema.core :as s :refer [Any Int both defschema either enum
                                       pred validate]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong command-id
  "A synchronized counter for Commands. There is no requirement to be either
   predictable or unpredictable, so this can be implemented as an incrementing
   global var."
  (AtomicLong. 0))

(def ^:private OPCODE-TABLE
  [[:app/help      VOID      "This help menu"]
   [:app/reload    VOID      "Reload configuration"]
   [:app/restart   VOID      "Restart application"]
   [:app/quit      VOID      "Quit application"]

   [:conn/remove   #{String} "Remove from registered connections"]
   [:conn/period   Int       "Set connection check period"]
   [:conn/trigger  VOID      "Re-check connections"]

   [:idle/add      MBMap     "Add to watched mboxes"]
   [:idle/remove   MBMap     "Remove from watched mboxes"]
   [:idle/set      MBMap     "Set watched mboxes"]
   [:idle/restart  VOID      "Restart IMAP connections"]

   [:notify/add    MBMap     "Add to notification mboxes"]
   [:notify/remove MBMap     "Remove from notification mboxes"]
   [:notify/set    MBMap     "Set notification mboxes"]

   [:sync          MBMap     "Synchronize given mailboxes"]
   [:sync/add      MBMap     "Add to periodic sync request"]
   [:sync/remove   MBMap     "Remove from periodic sync request"]
   [:sync/set      MBMap     "Set periodic sync request"]
   [:sync/period   Int       "Set sync period"]
   [:sync/term     VOID      "Terminate running mbsync processes"]
   [:sync/trigger  VOID      "Trigger periodic sync"]])

(def ^:private OPCODE->PAYLOAD
  (->> OPCODE-TABLE
       (mapcat (partial take 2))
       (apply hash-map)))

(def ^:private OPCODE->HELP
  (->> OPCODE-TABLE
       (mapcat (fn [[kw _ help]] [kw help]))
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

(defschema CommandSchema
  (both Command
        (pred #(do (validate (OPCODE->PAYLOAD (:opcode %))
                             (:payload %))
                   true)
              "Payload")))

(s/defn ->Command :- CommandSchema
  ([opcode]
   (->Command opcode nil))
  ([opcode  :- Opcode
    payload :- Any]
   (Command. opcode payload (.getAndIncrement command-id) (DateTime.))))

(s/defn parse-command-input :- (either CommandSchema String)
  "Try to parse user input as a Command, else return a help message."
  [s :- String]
  )
