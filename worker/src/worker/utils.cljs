(ns worker.utils
  (:require [cljs.js :as cljs]))

(def closure-base-path goog.basePath)
(def closure-base-file (str closure-base-path "/base.js"))
(def cljs-deps-file (str closure-base-path "../cljs_deps.js"))

(defn cljs->js [ns* code]
  (let [st (cljs/empty-state)]
  (binding [cljs.analyzer/*cljs-warning-handlers* []] ;; disable compiler warnings
    (cljs.js/eval-str st (str "(ns " ns* ")") nil {:eval cljs.js/js-eval :context :expr} identity)
    (cljs/compile-str
      st
      (pr-str code)
      nil
      {:ns (symbol ns*)}
      #(:value %)))))

(defn worker-body [ns* body]
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
   "goog.require('" ns* "');"

   "self.onmessage = function(e) {"
     body
   "};"))
