(ns worker.utils
  (:require [cljs.js :as cljs]))

(def closure-base-path goog.basePath)
(def closure-base-file (str closure-base-path "/base.js"))
(def cljs-deps-file (str closure-base-path "../cljs_deps.js"))

(defn cljs->js [code]
  (let [st (cljs/empty-state)]
    (cljs.js/eval-str st "(ns worker.worker)" nil {:eval cljs.js/js-eval :context :expr} identity)
    (cljs/compile-str
      st
      (pr-str code)
      nil
      {:ns 'worker.worker}
      #(:value %))))

(defn worker-body [body]
  (str
   "var CLOSURE_BASE_PATH = '" closure-base-path "';"
   "var CLOSURE_IMPORT_SCRIPT = (function(global) {"
   "return function(src) {"
      ;;"global['console'].log(src);"
      "global['importScripts'](src);"
      "return true;"
   "};"
   "})(self);"
   "importScripts('" closure-base-file "','" cljs-deps-file "');"
   "goog.require('cljs.core');"
   ;;@TODO: replace dinamicaly with current ns
   "goog.require('worker.worker');"

   "self.onmessage = function(e) {"
     body
   "};"))
