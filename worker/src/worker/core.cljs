(ns worker.core
  (:require [cljs.js :as cljs]
            [worker.worker :as w]))

(enable-console-print!)

(w/do-some)

;;(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
