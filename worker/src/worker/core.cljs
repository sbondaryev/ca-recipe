(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w]))

(defn ^:export main []
  (enable-console-print!)
  (println "start..")
  (w/do-some))
