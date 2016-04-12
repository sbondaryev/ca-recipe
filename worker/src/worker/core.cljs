(ns worker.core
  (:require [cljs.js :as cljs]))

(enable-console-print!)

(def b (new js/Blob (clj->js ["
var CLOSURE_BASE_PATH = 'http://localhost:3449/js/compiled/out/goog/';
var CLOSURE_IMPORT_SCRIPT = (function(global) {
  return function(src) {
    //global['console'].log(src);
    global['importScripts'](src);
    return true;
  };
})(self);

self.onmessage = function(e) {
  importScripts(
    'http://localhost:3449/js/compiled/out/goog/base.js'
    ,'http://localhost:3449/js/compiled/out/cljs_deps.js'
  )
  goog.require('cljs.core')

  cljs.core.enable_console_print_BANG_.call(null);
  cljs.core.println.call(null,e.data);
  postMessage(e.data);
};
"])))

(def w (new js/Worker (.createObjectURL js/URL b)))

(defn worker-print [message]
  (println message))

(println (cljs/compile-str (cljs/empty-state) "(println \"test\")" #(:value %)))

(.postMessage w "hello google closure worker")


;;(defonce app-state (atom {:text "Hello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
