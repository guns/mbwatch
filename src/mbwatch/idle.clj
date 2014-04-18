(ns mbwatch.idle
  "
     ─────── Command ────────┐
                             │
                             ▼                      ─┐
                       ┌────────────┐                │
                       │ IDLEMaster │                │
                       └─────┬──────┘                │
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
            [com.stuartsierra.component :refer [Lifecycle]]
            [mbwatch.command :refer [->Command]]
            [mbwatch.concurrent :refer [CHAN-SIZE sig-notify-all sig-wait
                                        thread-loop]]
            [mbwatch.config.mbsyncrc :refer [IMAPCredential]]
            [mbwatch.events :refer [->IDLEEvent ->IDLENewMessageEvent
                                    ->IMAPCommandError]]
            [mbwatch.logging :refer [->LogItem DEBUG Loggable
                                     log-with-timestamp!]]
            [mbwatch.types :as t :refer [ConnectionMapAtom NotifyMap
                                         NotifyMapAtom VOID Word]]
            [mbwatch.util :refer [url-for]]
            [schema.core :as s :refer [Any Int maybe]])
  (:import (clojure.lang IFn)
           (com.sun.mail.imap IMAPFolder IMAPStore)
           (com.sun.mail.util MailConnectException)
           (java.util Properties)
           (java.util.concurrent.atomic AtomicBoolean)
           (javax.mail AuthenticationFailedException Folder
                       FolderNotFoundException MessagingException Session)
           (javax.mail.event MessageCountListener)
           (mbwatch.events IMAPConnectionEvent)
           (org.joda.time DateTime)))

(def ^:private ^:const CONNECTION-POOL-SIZE "16")
(def ^:private ^:const IDLE-PERIOD (* 20 60 1000))

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
   log-chan        :- WritePort
   timeout         :- Int
   f               :- IFn]
  (let [{:keys [host port user pass ssl?]} imap-credential
        scheme (if ssl? "imaps" "imap")
        url (url-for scheme host user port)
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
      (catch AuthenticationFailedException e
        (log :badauth (str e)))
      ;; TODO: This is a subclass of MessagingException, but maybe it has more
      ;;       descriptive messages?
      (catch MailConnectException e
        (log :failure (str e)))
      (catch MessagingException e
        (log :failure (str e)))
      (finally
        (if (.isConnected store)
          (do (log :stop)
              (.close store)
              (log :disconnect))
          (log :lost))))))

(s/defn ^:private idle! :- VOID
  "IDLE on mbchan/mbox and queue :sync Commands on new messages. Blocks thread
   indefinitely. Signal exit-lock to exit."
  [imap-store :- IMAPStore
   mbchan     :- String
   mbox       :- String
   exit-lock  :- Object
   cmd-chan   :- WritePort
   log-chan   :- WritePort]
  (let [folder ^IMAPFolder (.getFolder imap-store mbox)
        url (str imap-store \/ mbox)
        status (AtomicBoolean. true)
        ;; A signal on exit-lock means GTFO
        exit-loop (future
                    (sig-wait exit-lock)
                    (.set status false)
                    (loop []
                      (sig-notify-all status)
                      (Thread/sleep 100)
                      (recur)))
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
            (sig-wait status IDLE-PERIOD)
            (future-cancel idle)
            (recur))))
      (catch FolderNotFoundException e
        (put! log-chan (->IMAPCommandError :folder-not-found url (str e))))
      (finally
        (future-cancel exit-loop)))))

(t/defrecord IDLEWorker
  []
  ; imap-credential log-chan timeout
  ; mbchan     :- String
  ; mbox       :- String
  ; exit-lock  :- Object
  ; cmd-chan   :- WritePort
  ; log-chan   :- WritePort
  )

(declare process-command)
(declare stop-workers!)

(t/defrecord IDLEMaster
  [mbchan->IMAPCredential :- {Word IMAPCredential}
   idle-map-atom          :- NotifyMapAtom
   timeout                :- Int
   cmd-chan-in            :- ReadPort
   cmd-chan-out           :- WritePort
   log-chan               :- WritePort
   status                 :- AtomicBoolean
   exit-fn                :- (maybe IFn)]

  Lifecycle

  (start [this]
    (log-with-timestamp! log-chan this)
    (let [c (thread-loop [worker-map {}]
              (if-some [cmd (when (.get status)
                              (<!! cmd-chan-in))]
                (recur (process-command this worker-map cmd))
                (stop-workers! (vals worker-map))))]
      (assoc this :exit-fn
             #(do (.set status false) ; Stop after current iteration
                  (<!! c)
                  (close! log-chan)   ; Close outgoing channels
                  ))))

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
   timeout                :- Int
   connections-atom       :- ConnectionMapAtom ; Read-only!
   cmd-chan-in            :- ReadPort]
  (strict-map->IDLEMaster
    {:mbchan->IMAPCredential mbchan->IMAPCredential
     :notify-map-atom (atom idle-map)
     :timeout timeout
     :connections-atom connections-atom
     :cmd-chan-in cmd-chan-in
     :cmd-chan-out (chan CHAN-SIZE)
     :log-chan (chan CHAN-SIZE)
     :status (AtomicBoolean. true)
     :exit-fn nil}))
