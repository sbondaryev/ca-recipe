(ns worker.worker (:require [cljs.js :as cljs]))


(def tm 1000)
(defn sleep [msec]
  (let [deadline (+ msec (.getTime (js/Date.)))]
    (while (> deadline (.getTime (js/Date.)))
      (* 1 1) ;;advanced mode
    )))

(def closure-base-path goog.basePath)
(def closure-base-file (str closure-base-path "/base.js"))
(def cljs-deps-file (str closure-base-path "../cljs_deps.js"))

(defn worker-body [fn-name]
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
   "goog.require('worker.worker');"

   "self.onmessage = function(e) {"
     "worker.worker.wrk();"
   "};"))

(defn worker-body-min [fn-name]
 (str
  "importScripts('file:///Users/sbondaryev/clojure/sandbox/worker/resources/public/js/compiled/worker-min.js');"
  "self.onmessage = function(e) {"
    "worker.worker.wrk();"
  "};"))


(defn func-name [f]
  (let [fstr (str f)
        fname (subs fstr (count "function ") (.indexOf fstr "("))
        fsplit (.split fname "$")]
    (.join fsplit ".")))


(defn ^:export wrk []
  (enable-console-print!)
  (println tm)
  (sleep tm)
  (println "test")
  ;;(throw (js/Error. "test"))
  )


(defn do-some []
(println goog.basePath)
  (let [a (worker-body (func-name wrk))]
    (println a)

    (def b (js/Blob. (clj->js [a])))

    (def w (js/Worker. (.createObjectURL js/URL b)))

    (.postMessage w nil)
    ))
(defn do-some [wrk]
  (let [a (worker-body (full-func-name wrk))
        b (js/Blob. (clj->js [a]))
        w (js/Worker. (.createObjectURL js/URL b))]
    (.postMessage w nil)))
