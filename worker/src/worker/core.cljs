(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w]))


(defn ^:export main []
  (enable-console-print!)
  (println "work1..")
  (w/do-some `wrk)
  (println "work2..")
  (w/do-some `wrk1)
  )
