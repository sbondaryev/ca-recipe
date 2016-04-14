(ns worker.worker
  (:require [cljs.js :as cljs]
            [worker.utils :as u])
  (:require-macros [worker.macros :as f]))
(def tm 500)
(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.))))))

(defn do-some []
  (let [a (f/code->str
    ;;@TODO use [] to set namespaces to load
    "worker.worker"
    (enable-console-print!)
    (sleep tm)
    (println "hello google closure worker"))]

    ;;(println (apply str (drop-last 2 (str `_))))
    (println a)

    (def b (js/Blob. (clj->js [a])))

    (def w (js/Worker. (.createObjectURL js/URL b)))

    (.postMessage w nil)
    ))
