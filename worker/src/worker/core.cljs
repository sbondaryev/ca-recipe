(ns worker.core
  (:require [cljs.js :as cljs]))

(enable-console-print!)


(def b (new js/Blob (clj->js ["self.addEventListener('message', function(e) {
console.log('in worker');
postMessage(e.data);
} ,false);"])))

(def w (new js/Worker (.createObjectURL js/URL b)))

(.addEventListener w "message" (fn [e] (println (.-data e))))

(println (cljs/compile-str (cljs/empty-state) "(defn j[x y] (+ x y))" #(:value %)))


(.postMessage w "test")


;;(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
