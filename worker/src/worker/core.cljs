(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w])
  (:require-macros [worker.macros :as wm]))

(def tm 2000)
(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.)))
      (* 1 1) ;;advanced mode
    )))

(defn ^:export wrk []
  (enable-console-print!)
  (println "process..")
  (sleep tm)
  (println "done!")
  {:prnt tm})

(defn ^:export wrk2 []
  (enable-console-print!)
  (println (* 2 tm))
  (sleep (* 3 tm))
  (println "test")
  {:time tm
    :prnt 100})

(defn do-some []
  (let [w (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))
        wmeta (meta (var wrk))
        result (atom)]
    (set! (.-onmessage w) #(reset! result (worker.worker/*deserialize* (.-data %))))
    (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))
    {:w w :result result}))

(defn ^:export main []
  (enable-console-print!)
  (def strt (.getTime (js/Date.)))

  (def w1 (do-some))
  (def w2 (do-some))
  (def w3 (do-some))
  (def w4 (do-some))
  (add-watch (:result w1) nil #(println (- (.getTime (js/Date.)) strt)))
  (add-watch (:result w2) nil #(println (- (.getTime (js/Date.)) strt)))
  (add-watch (:result w3) nil #(println (- (.getTime (js/Date.)) strt)))
  (add-watch (:result w4) nil #(println (- (.getTime (js/Date.)) strt)))
  (wrk)
  (println (- (.getTime (js/Date.)) strt))
  (wrk)
  (println (- (.getTime (js/Date.)) strt))
  (wrk)
  (println (- (.getTime (js/Date.)) strt))
  (wrk)
  (println (- (.getTime (js/Date.)) strt))

  )
