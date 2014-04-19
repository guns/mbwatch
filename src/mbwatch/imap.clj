(ns mbwatch.imap
  "
             ─────── Command ────────┐
                                     │
                                     ▼                      ─┐
   ┌┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┐       ┌────────────┐                │
   ┊ ConnectionMapAtom ├┄┄┄┄┄▷ │ IDLEMaster │                │
   └┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘       └─────┬──────┘                │
                                     │                       │
                          ┌──────────┴──────────┐            ├── Loggable ───▶
                          ▼                     ▼            │
                   ┌────────────┐         ┌────────────┐     │
                   │ IDLEWorker │    …    │ IDLEWorker │     │
                   └────────────┘         └────────────┘     │
                                                            ─┘
  "
  (:require [clojure.core.async :refer [<!! >!! chan close! put!]]
            [clojure.core.async.impl.protocols :refer [ReadPort WritePort]]
            [clojure.set :refer [intersection subset? union]]
            [com.stuartsierra.component :as comp :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE future-loop shutdown-future
                                        sig-notify-all sig-wait thread-loop]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.events :refer [->IDLEEvent ->IDLENewMessageEvent
                                    ->IMAPCommandError]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [ConnectionMapAtom MbTuple NotifyMap
                                         NotifyMapAtom VOID Word]]
            [mbwatch.util :refer [map-mbtuples notify-map-diff
                                  notify-map-disj reduce-mbtuples url-for]]
            [schema.core :as s :refer [Any Int defschema maybe]])
  (:import (clojure.lang IFn)
           (com.sun.mail.imap IMAPFolder IMAPStore)
           (com.sun.mail.util MailConnectException)
           (java.util Properties)
           (java.util.concurrent.atomic AtomicBoolean)
           (javax.mail AuthenticationFailedException Folder
                       FolderNotFoundException MessagingException Session)
           (javax.mail.event MessageCountListener)
           (mbwatch.command Command)
           (mbwatch.events IMAPConnectionEvent)
           (org.joda.time DateTime)))

(def ^:private ^:const CONNECTION-POOL-SIZE "16")
(def ^:private ^:const IDLE-PERIOD (* 20 60 1000))
(def ^:private ^:const IMAP-SHUTDOWN-TIMEOUT 5000)

(s/defn ^:private ->IMAPProperties :- Properties
  "Return a copy of system properties with mail.imap(s) entries."
  [timeout :- Int]
  (let [props ^Properties (.clone (System/getProperties))
        t (str timeout)]
    (doseq [[k v] [["connectiontimeout" t] ; Socket connect
                   ["timeout" t]           ; Socket read
                   ["writetimeout" t]      ; Socket write (+1 thread used)
                   ["connectionpooltimeout" t]
                   ["connectionpoolsize" CONNECTION-POOL-SIZE]]]
      (.setProperty props (str "mail.imap" k) v)
      (.setProperty props (str "mail.imaps" k) v))
    props))

(s/defn ^:private with-imap-connection :- Any
  [imap-credential :- IMAPCredential
   label           :- Any
   log-chan        :- WritePort
   timeout         :- Int
   f               :- IFn]
  (let [{:keys [host port user pass ssl?]} imap-credential
        scheme (if ssl? "imaps" "imap")
        url (cond-> (url-for scheme host user port)
              label (str label))
        store (-> (->IMAPProperties timeout)
                  (Session/getDefaultInstance)
                  (.getStore scheme))
        log (fn log
              ([type] (log type nil))
              ([type err] (put! log-chan (IMAPConnectionEvent. type url err (DateTime.)))))]
    (try
      (log :start)
      (.connect store host port user pass)
      (log :success)
      (f store)
      (catch AuthenticationFailedException e (log :badauth (str e)))
      (catch MailConnectException e (log :failure (str e))) ;; TODO: Do we want this?
      (catch MessagingException e (log :failure (str e)))
      (finally
        (if (.isConnected store)
          (do (log :stop)
              (.close store)
              (log :disconnect))
          (log :lost))))))

(s/defn ^:private idle! :- VOID
  "IDLE on mbchan/mbox and queue :sync Commands on new messages. Blocks thread
   indefinitely. Signal status to restart. Set status to false then signal it
   to exit."
  [imap-store :- IMAPStore
   mbchan     :- String
   mbox       :- String
   status     :- AtomicBoolean
   cmd-chan   :- WritePort
   log-chan   :- WritePort]
  (let [folder ^IMAPFolder (.getFolder imap-store mbox)
        url (str imap-store \/ mbox)
        handler (reify MessageCountListener
                  (messagesAdded [this ev]
                    (put! log-chan (->IDLENewMessageEvent (count (.getMessages ev)) url))
                    (>!! cmd-chan (->Command :sync {mbchan [mbox]}))))]
    (try
      (.open folder Folder/READ_ONLY)
      (.addMessageCountListener folder handler)
      (put! log-chan (->IDLEEvent url))
      (loop []
        (when (.get status)
          (let [idle (future (.idle folder))]
            ;; The IDLE command may be terminated after 30 minutes
            (try
              (sig-wait status IDLE-PERIOD)
              (finally
                (future-cancel idle)))
            (recur))))
      (catch FolderNotFoundException e
        (put! log-chan (->IMAPCommandError :folder-not-found url (str e)))))))

(t/defrecord ^:private IDLEWorker
  [mbchan          :- String
   mbox            :- String
   imap-credential :- IMAPCredential
   timeout         :- Int
   cmd-chan        :- WritePort
   log-chan        :- WritePort
   status          :- AtomicBoolean
   exit-fn         :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [label (format " [%s/%s]" mbchan mbox)
          f (future-loop []
              (when (.get status)
                (with-imap-connection imap-credential label log-chan timeout
                  (fn [store]
                    (idle! store mbchan mbox status cmd-chan log-chan)))
                (recur)))]
      ;; IDLEMaster will close the shared outgoing channels
      (assoc this :exit-fn
             #(do (.set status false)     ; Stop after current iteration
                  (sig-notify-all status) ; Stop IMAP connection
                  (shutdown-future f IMAP-SHUTDOWN-TIMEOUT)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s IDLEWorker for %s/%s"
                            (if exit-fn "↓ Stopping" "↑ Starting")
                            mbchan mbox))))

(declare process-command)
(declare start-workers)
(declare stop-workers!)

(t/defrecord IDLEMaster
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   idle-map-atom          :- NotifyMapAtom
   connections-atom       :- ConnectionMapAtom
   timeout                :- Int
   cmd-chan-in            :- ReadPort
   cmd-chan-out           :- WritePort
   log-chan               :- WritePort
   status                 :- AtomicBoolean
   exit-fn                :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [c (thread-loop [worker-map (start-workers
                                       this {} (map-mbtuples @idle-map-atom))]
              (if-some [cmd (when (.get status) (<!! cmd-chan-in))]
                ;; Convey commands ASAP
                (do (>!! cmd-chan-out cmd)
                    (recur (process-command this worker-map cmd)))
                (stop-workers! (vals worker-map))))]
      (assoc this :exit-fn
             #(do (.set status false)   ; Stop after current iteration
                  (<!! c)
                  (close! cmd-chan-out) ; Close outgoing channels
                  (close! log-chan)))))

  (stop [this]
    (log-with-timestamp! log-chan this)
    (exit-fn)
    (assoc this :exit-fn nil))

  Loggable

  (log-level [_] DEBUG)

  (log-item [this]
    (->LogItem this (format "%s IDLEMaster" (if exit-fn "↓ Stopping" "↑ Starting")))))

(s/defn ->IDLEMaster :- IDLEMaster
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   idle-map               :- NotifyMap
   connections-atom       :- ConnectionMapAtom ; Read-only!
   timeout                :- Int
   cmd-chan-in            :- ReadPort]
  (strict-map->IDLEMaster
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :idle-map-atom (atom idle-map)
     :connections-atom connections-atom
     :timeout timeout
     :cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :status (AtomicBoolean. true)
     :exit-fn nil}))

(s/defn ^:private ->IDLEWorker :- IDLEWorker
  [idle-master :- IDLEMaster
   mbchan      :- String
   mbox        :- String]
  (let [{:keys [mbchan->IMAPCredential timeout cmd-chan-out log-chan]} idle-master]
    (strict-map->IDLEWorker
      {:mbchan mbchan
       :mbox mbox
       :imap-credential (mbchan->IMAPCredential mbchan)
       :timeout timeout
       :cmd-chan cmd-chan-out
       :log-chan log-chan
       :status (AtomicBoolean. true)
       :exit-fn nil})))

(defschema IDLEWorkerMap
  {MbTuple IDLEWorker})

(s/defn ^:private stop-workers! :- VOID
  [workers :- [IDLEWorker]]
  (dorun
    (pmap comp/stop workers)))

(s/defn ^:private start-workers :- IDLEWorkerMap
  [idle-master :- IDLEMaster
   worker-map  :- IDLEWorkerMap
   mbtuples    :- #{MbTuple}]
  {:pre [(empty? (intersection mbtuples (set (keys worker-map))))]}
  (reduce
    (fn [m [mbchan mbox]]
      (assoc m [mbchan mbox] (.start (->IDLEWorker idle-master mbchan mbox))))
    worker-map mbtuples))

(s/defn ^:private stop-and-start! :- IDLEWorkerMap
  [idle-master :- IDLEMaster
   worker-map  :- IDLEWorkerMap
   Δ-          :- #{MbTuple}
   Δ+          :- #{MbTuple}]
  {:pre [(subset? Δ- (set (keys worker-map)))]}
  (let [f (future (stop-workers! (vals (select-keys worker-map Δ-))))
        m (cond-> worker-map
            (seq Δ-) (as-> m (apply dissoc m Δ-))
            (seq Δ+) (as-> m (start-workers idle-master m Δ+)))]
    (>!! (:cmd-chan-out idle-master)
         (->Command :sync (reduce-mbtuples Δ+)))
    @f
    m))

(s/defn ^:private swap-stop-and-start! :- IDLEWorkerMap
  [idle-master :- IDLEMaster
   worker-map  :- IDLEWorkerMap
   f           :- IFn]
  (let [{:keys [idle-map-atom cmd-chan-out]} idle-master
        imap₀ @idle-map-atom
        imap₁ (f idle-map-atom)
        [Δ- Δ+] (notify-map-diff imap₀ imap₁)]
    (stop-and-start! idle-master worker-map Δ- Δ+)))

(s/defn ^:private process-command :- IDLEWorkerMap
  [idle-master :- IDLEMaster
   worker-map  :- IDLEWorkerMap
   command     :- Command]
  (case (:opcode command)
    :idle/add (swap-stop-and-start!
                idle-master worker-map
                #(swap! % (partial merge-with union) (:payload command)))
    :idle/remove (swap-stop-and-start!
                   idle-master worker-map
                   #(swap! % (partial notify-map-disj) (:payload command)))
    :idle/set (swap-stop-and-start!
                idle-master worker-map
                #(reset! % (:payload command)))
    :idle/restart (let [mbtuples (set (keys worker-map))]
                    (stop-workers! (vals worker-map))
                    (stop-and-start! idle-master {} #{} mbtuples))
    worker-map))
