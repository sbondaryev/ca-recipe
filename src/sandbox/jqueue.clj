(ns sandbox.jqueue
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn pusher [q n]
  (loop [i 0]
    (when (< i n)
      (.put q i)
      (recur (inc i))))
  (.put q :END))

(defn popper [q]
  (loop [items []]
    (let [item (.take q)]
      (if (= item :END)
        item
        (recur (conj items item))))))

(defn flow [n]
  (let [q (LinkedBlockingQueue.)
        consumer (future (papper q))
        begin (System/currentTimeMillis)
        producer (future (pusher q n))
        received @cunsumer
        end (System/currentTimeMillis)]
    (println "Received:" (count received) "in" (- end begin) "ms")))
