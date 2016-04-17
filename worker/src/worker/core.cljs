(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w]))

(def tm 1000)
(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.)))
      (* 1 1) ;;advanced mode
    )))

(defn ^:export wrk []
  (enable-console-print!)
  (println tm)
  (sleep tm)
  (println "test"))

(defn ^:export wrk1 []
  (enable-console-print!)
  (println 3000)
  (sleep 3000)
  (println "test2"))


(defn ^:export main []
  (enable-console-print!)
  (println "work1..")
  (w/do-some `wrk)
  (println "work2..")
  (w/do-some `wrk1)
  )
