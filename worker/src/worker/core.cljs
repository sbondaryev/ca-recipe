(ns worker.core
  (:require [cljs.js]
            [worker.worker :as w])
  (:require-macros [worker.macros :as wm]))

(def tm 1000)
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

(defn ^:export main []
  (enable-console-print!)
  (println "work1..")
;;  (println (w/js-func-name wrk))
;;  (wm/do-some 1)
    (w/do-some (meta (var wrk)))
;;  (println "work2..")
;;  (w/do-some `wrk2)
  )
