(ns worker.dispatch
  (:require [worker.buffer :as buffer]
            [worker.worker :as worker]
            [goog.async.nextTick]))

(def tasks (buffer/ring-buffer 32))
(def running? false)
(def queued? false)

(def TASK_BATCH_SIZE 1024)

(declare queue-dispatcher)

(defn process-messages []
  (set! running? true)
  (set! queued? false)
  (loop [count 0]
    (println "but i try")
    (if-let [w (worker/get-worker)]
      (let [m (.pop tasks)]
        (when-not (nil? m)
          (worker/lock-worker w)
          (m w)
          (when (< count TASK_BATCH_SIZE)
            (recur (inc count)))))))
  (set! running? false)
  (when (> (.-length tasks) 0)
    (queue-dispatcher)))

(defn queue-dispatcher []
  (when-not (and queued? running?)
    (set! queued? true)
    (goog.async.nextTick process-messages)))

(defn run [f]
  (.unbounded-unshift tasks f)
  (queue-dispatcher))

(defn queue-delay [f delay]
  (js/setTimeout f delay))
