(ns worker.worker
  (:require [cljs.js :as cljs]
            [worker.utils :as u])
  (:require-macros [worker.macros :as f]))

(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.))))))

(defn do-some []
  (let [a (f/code->str
    (enable-console-print!)
    (worker.worker/sleep 500)
    (println "hello google closure worker"))]

    (println a)

    (def b (js/Blob. (clj->js [a])))

    (def w (js/Worker. (.createObjectURL js/URL b)))

    (.postMessage w nil)
    ))
