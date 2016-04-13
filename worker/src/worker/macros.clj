(ns worker.macros
  (:require [cljs.js :as cljs]))

(def js-url "http://localhost:3449/js/compiled/out/")
(def closure-base-path (str js-url "goog/"))
(def closure-base-file (str closure-base-path "/base.js"))
(def cljs-deps-file (str js-url "/cljs_deps.js"))

(defmacro worker-body [body]
  `(str
   "var CLOSURE_BASE_PATH = '" ~closure-base-path "';"
   "var CLOSURE_IMPORT_SCRIPT = (function(global) {"
   "return function(src) {"
      ;;"global['console'].log(src);"
      "global['importScripts'](src);"
      "return true;"
   "};"
   "})(self);"

   "self.onmessage = function(e) {"
     "importScripts('" ~closure-base-file "','" ~cljs-deps-file "');"
     "goog.require('cljs.core');"
     ~body
   "};"))

(defmacro compile [code-str] 
  `(cljs/compile-str (cljs/empty-state) ~code-str #(:value %)))

(defmacro code->str [& body]
  `(-> (pr-str  '(do ~@body))
       (worker.macros/compile)
       (worker-body)))
