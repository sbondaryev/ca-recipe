(ns worker.core
  (:require [cljs.js :as cljs])
  (:require-macros [worker.macros :as f]))

(enable-console-print!)

(def a (f/code->str
        (enable-console-print!)
        (println "sdfsd")))

(def b (new js/Blob (clj->js [a])))

(def w (new js/Worker (.createObjectURL js/URL b)))

;;(println (f/worker-body "ertert"))
;;  (println (f/code->str (println "sdfsd")))
;;(println (cljs/compile-str (cljs/empty-state) "(println \"test\") (println \"rest\")" #(:value %)))

(.postMessage w "hello google closure worker")


;;(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
