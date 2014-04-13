(ns mbwatch.concurrent
  (:require [clojure.core.async :refer [thread]]
            [mbwatch.types :as t :refer [VOID]]
            [mbwatch.util :refer [catch-print]]
            [schema.core :as s :refer [Int defschema maybe pred]])
  (:import (clojure.lang Atom)))

(def ^:const CHAN-SIZE
  "4K ought to be enough for anybody."
  0x1000)

(defmacro future-catch-print
  {:requires [#'catch-print]}
  [& body]
  `(future
     (catch-print
       ~@body)))

(defmacro future-loop
  {:requires [#'catch-print]}
  [bindings & body]
  `(future
     (catch-print
       (loop ~bindings
         ~@body))))

(defmacro thread-loop
  {:requires [#'thread #'catch-print]}
  [bindings & body]
  `(thread
     (catch-print
       (loop ~bindings
         ~@body))))

(s/defn sig-wait :- VOID
  ([lock :- Object]
   (locking lock
     (.wait lock)))
  ([lock    :- Object
    timeout :- long]
   (when (pos? timeout)
     (locking lock
       (.wait lock timeout)))))

(s/defn sig-notify :- VOID
  [lock :- Object]
  (locking lock
    (.notify lock)))

(s/defn sig-notify-all :- VOID
  [lock :- Object]
  (locking lock
    (.notifyAll lock)))

(t/defrecord ^:private Timer
  [period :- long
   alarm  :- long])

(s/defn ->Timer :- Timer
  "Construct a new Timer with sane values. If trigger-now? is true, the alarm
   is set set to the current time."
  [period       :- long
   trigger-now? :- Boolean]
  (if (pos? period)
    (let [now (System/currentTimeMillis)]
      (Timer. period (if trigger-now? now (+ now period))))
    (Timer. 0 0)))

(defschema TimerAtom
  (pred #(and (instance? Atom %)
              (instance? Timer @%))
        "TimerAtom"))

(s/defn set-alarm! :- Timer
  "Set the :alarm entry ms-forward in a Timer. The 1-arity version sets the
   value to the current time + :period. When :period or ms-forward is zero,
   the :alarm is set to zero."
  ([timer-atom :- TimerAtom]
   (set-alarm! timer-atom nil))
  ([timer-atom :- TimerAtom
    ms-forward :- (maybe Int)]
   (swap! timer-atom
          (fn [timer Δt now]
            (let [Δt (max 0 (or Δt (:period timer)))]
              (assoc timer :alarm (if (zero? Δt) 0 (+ now Δt)))))
          ms-forward (System/currentTimeMillis))))

(s/defn sig-wait-timer :- VOID
  "Wait for signals on timer or wake up at :alarm time. If the value of :alarm
   has changed in the meantime, wait on timer again until the new alarm time.

   If the value of :alarm is zero or less, the thread waits indefinitely."
  [timer-atom :- TimerAtom]
  (loop [alarm (:alarm @timer-atom)]
    (if (pos? alarm)
      (sig-wait timer-atom (- alarm (System/currentTimeMillis)))
      (sig-wait timer-atom))
    (let [alarm' (:alarm @timer-atom)]
      (when-not (= alarm alarm')
        (recur alarm')))))

(s/defn ^:private update-timer* :- Timer
  [timer      :- Timer
   new-period :- Int]
  (let [new-period (max 0 new-period)]
    (if (= new-period (:period timer))
      timer
      (assoc timer
             :period new-period
             :alarm (cond
                      (zero? new-period) 0
                      (zero? (:alarm timer)) (+ new-period (System/currentTimeMillis))
                      :else (max (+ new-period (- (:alarm timer) (:period timer)))
                                 (System/currentTimeMillis)))))))

(s/defn update-timer! :- Boolean
  "Swap in a new value of :period and set :alarm accordingly.

   If new-period is zero, :alarm is also set to zero to signal the off state.
   Otherwise, :alarm is set to the greater of the adjusted :alarm value and
   the current time. Negative values of new-period are interpreted as zero.

   Returns true if the Timer value changed, and false if it did not."
  [timer      :- TimerAtom
   new-period :- Int]
  (let [timer₀ @timer]
    (not= timer₀ (swap! timer update-timer* new-period))))
