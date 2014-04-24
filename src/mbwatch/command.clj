(ns mbwatch.command
  "Mbwatch components are configured and controlled via Commands."
  (:require [clojure.string :as string]
            [mbwatch.logging :refer [DEBUG Loggable]]
            [mbwatch.mbmap :refer [parse-mbline]]
            [mbwatch.trie :refer [EMPTY-TRIE-NODE add-substring-aliases
                                  lookup]]
            [mbwatch.types :as t :refer [MBMap VOID tuple]]
            [schema.core :as s :refer [Any Int Schema both defschema either
                                       enum maybe pred validate]])
  (:import (java.util.concurrent.atomic AtomicLong)
           (mbwatch.logging LogItem)
           (org.joda.time DateTime)))

(def ^:private ^AtomicLong command-id
  "A synchronized counter for Commands. There is no requirement to be either
   predictable or unpredictable, so this can be implemented as an incrementing
   global var."
  (AtomicLong. 0))

(t/defrecord ^:private OpcodeMeta
  [payload-type :- Schema
   user-command :- String
   help         :- String])

(def ^:private OPCODE-TABLE
  {:app/help      (OpcodeMeta. VOID      "help"               "This help menu")
   :app/status    (OpcodeMeta. VOID      "info"               "Query info of application")
   :app/reload    (OpcodeMeta. VOID      "reload"             "Reload configuration")
   :app/restart   (OpcodeMeta. VOID      "RESTART"            "Restart application")
   :app/quit      (OpcodeMeta. VOID      "quit"               "Quit application")

   :conn/remove   (OpcodeMeta. #{String} "connection remove"  "Remove from registered connections")
   :conn/period   (OpcodeMeta. Int       "connection period"  "Set connection check period")
   :conn/trigger  (OpcodeMeta. VOID      "connection trigger" "Re-check connections")

   :idle/add      (OpcodeMeta. MBMap     "idle add"           "Add to watched mboxes")
   :idle/remove   (OpcodeMeta. MBMap     "idle remove"        "Remove from watched mboxes")
   :idle/set      (OpcodeMeta. MBMap     "idle set"           "Set watched mboxes")
   :idle/restart  (OpcodeMeta. VOID      "idle RESTART"       "Restart IMAP connections")

   :notify/add    (OpcodeMeta. MBMap     "notify add"         "Add to notification mboxes")
   :notify/remove (OpcodeMeta. MBMap     "notify remove"      "Remove from notification mboxes")
   :notify/set    (OpcodeMeta. MBMap     "notify set"         "Set notification mboxes")

   :sync          (OpcodeMeta. MBMap     "SYNC"               "Synchronize given mailboxes")
   :sync/add      (OpcodeMeta. MBMap     "sync add"           "Add to periodic sync request")
   :sync/remove   (OpcodeMeta. MBMap     "sync remove"        "Remove from periodic sync request")
   :sync/set      (OpcodeMeta. MBMap     "sync set"           "Set periodic sync request")
   :sync/period   (OpcodeMeta. Int       "sync period"        "Set sync period")
   :sync/term     (OpcodeMeta. VOID      "TERMINATE"          "Terminate running mbsync processes")
   :sync/trigger  (OpcodeMeta. VOID      "trigger"            "Trigger periodic sync")})

(def ^:private USER-COMMAND-TRIE
  (reduce-kv
    (fn [node op opmeta]
      (let [cmd (:user-command opmeta)]
        (add-substring-aliases node cmd op)))
    EMPTY-TRIE-NODE OPCODE-TABLE))

(defschema ^:private Opcode
  (apply enum (keys OPCODE-TABLE)))

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
        (pred #(do (validate (:payload-type (OPCODE-TABLE (:opcode %)))
                             (:payload %))
                   true)
              "Payload")))

(s/defn ->Command :- CommandSchema
  ([opcode]
   (->Command opcode nil))
  ([opcode  :- Opcode
    payload :- Any]
   (Command. opcode payload (.getAndIncrement command-id) (DateTime.))))

(s/defn ^:private parse-command-input* :- (maybe (tuple #{Opcode} [String]))
  [input :- String]
  (let [words (string/split input #"\s+")]
    (let [ops (lookup USER-COMMAND-TRIE (first words))
          nops (count ops)
          nwords (count words)]
      (if (nil? ops)
        nil
        (if (and (> nwords 1) (> nops 1))
          (when-let [ops (lookup USER-COMMAND-TRIE (string/join " " (take 2 words)))]
            [ops (drop 2 words)])
          [ops (rest words)])))))

(s/defn parse-command-input :- (either CommandSchema String)
  "Try to parse user input as a Command, else return a help message."
  [input :- String]
  (let [[ops args] (parse-command-input* input)
        nops (count ops)]
    (cond (= nops 0) (str "Command unrecognized: " (pr-str input))
          (> nops 1) (str "Ambiguous command: One of "
                          (string/join
                            ", "
                            (mapv (comp pr-str :user-command OPCODE-TABLE) ops)))
          :else
          (let [op (first ops)
                op-str (pr-str op)
                {:keys [payload-type]} (OPCODE-TABLE op)]
            (condp = payload-type
              VOID (if (seq args)
                     (str op-str " takes no arguments")
                     (->Command op nil))
              #{String} (->Command op (set args))
              Int (if (or (not= (count args) 1)
                          (not (re-find #"\A\d+\z" (first args))))
                    (str op-str " expects a single integer argument")
                    (->Command op (first args)))
              MBMap (if (empty? args)
                      (str op-str " expects arguments of the form channel:box,…")
                      (->Command op (parse-mbline (string/join \space args)))))))))
