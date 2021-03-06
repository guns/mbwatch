(ns mbwatch.command
  "Mbwatch components are configured and controlled via Commands."
  (:require [clojure.string :as string]
            [mbwatch.logging.levels :refer [DEBUG]]
            [mbwatch.logging.protocols :refer [Loggable]]
            [mbwatch.logging]
            [mbwatch.mbmap :refer [parse-mbline]]
            [mbwatch.time :refer [parse-ms]]
            [mbwatch.trie :refer [EMPTY-TRIE-NODE add-command-aliases lookup]]
            [mbwatch.types :as t :refer [MBMap MBMap+ VOID tuple]]
            [mbwatch.util :refer [make-table]]
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

(t/defrecord OpcodeMeta
  [payload-type :- Schema
   user-command :- String
   help         :- String])

(let [t [:app/help        (OpcodeMeta. VOID      "help"            "This help menu")
         :app/clear       (OpcodeMeta. VOID      "CLEAR"           "Clear password cache")
         :app/status      (OpcodeMeta. VOID      "INFO"            "Print application status")
         :app/reload      (OpcodeMeta. VOID      "reload"          "Reload configuration")
         :app/restart     (OpcodeMeta. VOID      "RESTART"         "Restart application")
         :app/quit        (OpcodeMeta. VOID      "quit"            "Quit application")

         :idle/add        (OpcodeMeta. MBMap+    "idle add"        "Add to watched mboxes")
         :idle/remove     (OpcodeMeta. MBMap     "idle remove"     "Remove from watched mboxes")
         :idle/set        (OpcodeMeta. MBMap+    "idle set"        "Set watched mboxes")
         :idle/clear      (OpcodeMeta. VOID      "idle clear"      "Clear watched mboxes")
         :idle/restart    (OpcodeMeta. VOID      "idle RESTART"    "Restart IMAP connections")

         :notify/add      (OpcodeMeta. MBMap     "notify add"      "Add to notification mboxes")
         :notify/remove   (OpcodeMeta. MBMap     "notify remove"   "Remove from notification mboxes")
         :notify/set      (OpcodeMeta. MBMap     "notify set"      "Set notification mboxes")
         :notify/clear    (OpcodeMeta. VOID      "notify clear"    "Clear notification mboxes")

         :blacklist/set   (OpcodeMeta. MBMap     "blacklist set"   "Set notification blacklist")
         :blacklist/clear (OpcodeMeta. VOID      "blacklist clear" "Clear notification blacklist")

         :sync            (OpcodeMeta. MBMap     "SYNC"            "Synchronize given mailboxes")
         :sync/add        (OpcodeMeta. MBMap     "sync add"        "Add to periodic sync request")
         :sync/remove     (OpcodeMeta. MBMap     "sync remove"     "Remove from periodic sync request")
         :sync/set        (OpcodeMeta. MBMap     "sync set"        "Set periodic sync request")
         :sync/clear      (OpcodeMeta. VOID      "sync clear"      "Clear periodic sync request")
         :sync/period     (OpcodeMeta. Int       "sync period"     "Set sync period")
         :sync/trigger    (OpcodeMeta. VOID      "trigger"         "Trigger periodic sync")
         :sync/term       (OpcodeMeta. VOID      "TERMINATE"       "Terminate running mbsync processes")

         :conn/remove     (OpcodeMeta. #{String} "conn remove"     "Remove channels from registered connections")
         :conn/clear      (OpcodeMeta. VOID      "conn clear"      "Clear registered connections")
         :conn/period     (OpcodeMeta. Int       "conn period"     "Set connection check period")
         :conn/trigger    (OpcodeMeta. VOID      "conn trigger"    "Re-check connections")]]

  (def OPCODE-HELP
    (str (make-table
           ["Command" "Arguments" "Description"]
           (mapv (fn [{:keys [payload-type user-command help]}]
                   [user-command
                    (condp = payload-type
                      VOID ""
                      MBMap "channel[:box,…]"
                      MBMap+ "channel:box[,…]"
                      Int "Number + TIME UNIT"
                      #{String} "channel …")
                    help])
                 (take-nth 2 (rest t))))
         "\n"
         "\n* TIME UNIT: One of d, h, m, s, ms; minutes are the default unit"
         "\n* Commands may be abbreviated: `i a home:INBOX` -> `idle add home:INBOX`"))

  (def OPCODE-TABLE (apply hash-map t)))

(def ^:private USER-COMMAND-TRIE
  (reduce-kv
    (fn [node op opmeta]
      (let [cmd (:user-command opmeta)]
        (add-command-aliases node cmd op)))
    EMPTY-TRIE-NODE OPCODE-TABLE))

(defschema ^:private Opcode
  (apply enum (keys OPCODE-TABLE)))

(t/defrecord Command
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
  (let [words (string/split (string/trim input) #"\s+")
        first-word (first words)]
    (if (empty? first-word)
      [#{:app/status} nil]
      (let [ops (lookup USER-COMMAND-TRIE first-word)
            nops (count ops)
            nwords (count words)]
        (when (some? ops)
          (if (and (> nwords 1) (> nops 1))
            (when-some [ops (lookup USER-COMMAND-TRIE (string/join " " (take 2 words)))]
              [ops (drop 2 words)])
            [ops (rest words)]))))))

(s/defn parse-command-input :- (either CommandSchema String)
  "Try to parse user input as a Command, else return a help message."
  [input :- String]
  (let [[ops args] (parse-command-input* input)
        nops (count ops)]
    (cond
      (= nops 0) (str "Command unrecognized: " (pr-str input))
      (> nops 1) (str "Ambiguous command: One of "
                      (string/join
                        ", "
                        (mapv (comp pr-str :user-command OPCODE-TABLE) ops)))
      :else
      (let [op (first ops)
            {:keys [payload-type user-command]} (OPCODE-TABLE op)
            op-str (pr-str user-command)]
        (condp = payload-type
          VOID (if (seq args)
                 (str op-str " takes no arguments")
                 (->Command op nil))
          MBMap (if (empty? args)
                  (str op-str " expects arguments of the form channel:box,…")
                  (->Command op (parse-mbline (string/join \space args))))
          MBMap+ (let [mbmap (parse-mbline (string/join \space args))]
                   (if (or (empty? args) (s/check MBMap+ mbmap))
                     (str op-str " requires arguments in the form of channel:box[,…]")
                     (->Command op mbmap)))
          Int (if (empty? args)
                (str op-str " expects a time argument")
                (try
                  (->Command op (parse-ms (string/join \space args)))
                  (catch Throwable e
                    (str e))))
          #{String} (->Command op (set args)))))))
