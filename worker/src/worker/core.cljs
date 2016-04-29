(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w]
            [worker.dispatch :as dispatch])
  (:require-macros [worker.macros :as wm]))

(def tm 2000)
(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.)))
      (* 1 1) ;;advanced mode
    )))

(declare do-some-wrk2)

(defn ^:export wrk []
  (enable-console-print!)
  (println "worker1")
  (do-some-wrk2))

(defn ^:export wrk2 []
  (enable-console-print!)
  (println "worker2")
  (+ 1 1))

(defn do-some-wrk []
  (js/Promise. (fn [res rej]
    (let [w (w/Worker)
          wmeta (meta (var wrk))]
      ;;(.addEventListener w "message" #(res (worker.worker/*deserialize* (.-data %))))
      (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))))))

(defn do-some-wrk2 []
  (js/Promise. (fn [res rej]
    (dispatch/run
      (fn [w]
        (let [wmeta (meta (var wrk2))]
          (.addEventListener w "message" #(do
            (set! (.-onmessage w) nil)
            (res (worker.worker/*deserialize* (.-data %)))))
          (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))
      ))))))


(defn ^:export main []
  (enable-console-print!)
(w/add-worker)
(w/add-worker)
(.then (do-some-wrk2) #(println %))
(.then (do-some-wrk2) #(println %))
(.then (do-some-wrk2) #(println %))
(.then (do-some-wrk2) #(println %))
(.then (do-some-wrk2) #(println %))
(.then (do-some-wrk2) #(println %))


  )
