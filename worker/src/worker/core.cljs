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

(defn do-some []
  (let [w (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))]
    (set! (.-onmessage w) #(println "test"))
    (.postMessage w nil)))

(defn ^:export wrk []
  (enable-console-print!)
  (println "process..")
  (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))
  (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))
  (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))
;;  (.then (do-some) #(println "subworker"))
  (sleep 10000)
  (println "done!")
  {:prnt tm})

(defn ^:export wrk2 []
  (enable-console-print!)
  (println (* 2 tm))
  (sleep (* 3 tm))
  (println "test")
  {:time tm
    :prnt 100})



(defn do-some-pr []
  (js/Promise. (fn [res rej]
    (let [w (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))
          wmeta (meta (var wrk))]
      (set! (.-onmessage w) #(res (worker.worker/*deserialize* (.-data %))))
      (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))))))

(defn ^:export main []
  (enable-console-print!)
  (def strt (.getTime (js/Date.)))

  (.then (do-some-pr) #(println %))

  ; (def w1 (do-some))
  ; (def w2 (do-some))
  ; (def w3 (do-some))
  ; (def w4 (do-some))
  ; (add-watch (:result w1) nil #(println (- (.getTime (js/Date.)) strt)))
  ; (add-watch (:result w2) nil #(println (- (.getTime (js/Date.)) strt)))
  ; (add-watch (:result w3) nil #(println (- (.getTime (js/Date.)) strt)))
  ; (add-watch (:result w4) nil #(println (- (.getTime (js/Date.)) strt)))
  ; (wrk)
  ; (println (- (.getTime (js/Date.)) strt))
  ; (wrk)
  ; (println (- (.getTime (js/Date.)) strt))
  ; (wrk)
  ; (println (- (.getTime (js/Date.)) strt))
  ; (wrk)
  ; (println (- (.getTime (js/Date.)) strt))

  )
