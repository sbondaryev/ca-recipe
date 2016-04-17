(ns worker.worker (:require [goog.dom :as dom]))

(declare cljs-output-file)

(def *closure-base-path* goog/basePath)
(def *closure-base-file* (str *closure-base-path* "base.js"))
(def *document* {
  :getElementsByTagName "function() {return [];}"
})

(defn scripts-src []
  (let [scripts (.getElementsByTagName (dom/getDocument) "SCRIPT")]
    (->> (for [i (range (.-length scripts))] (aget scripts i))
         (remove #(empty? (.-src %)))
         (map #(.-src %)))))

(defn cljs-output-file []
  (if-not (empty? *closure-base-path*)
    (str *closure-base-path* "../cljs_deps.js")
    (first (scripts-src))))

(def *cljs-output-file* (cljs-output-file))

(defn serialize [obj]
  (->> obj
    (map (fn [[key val]] (str (name key) ":" val)))
    (interpose ",")
    (#(str "{" (apply str %) "}"))))

(defn worker-body [[ns* fn*]]
  (let [
    multi-loader (str
     "var CLOSURE_BASE_PATH = '" *closure-base-path* "';"
     "var CLOSURE_IMPORT_SCRIPT = (function(global) {"
     "return function(src) {"
        ;;"global['console'].log(src);"
        "global['importScripts'](src);"
        "return true;"
     "};"
     "})(self);"
     "importScripts('" *closure-base-file* "','" *cljs-output-file* "');"
     "goog.require('" ns* "');")
    single-loader (str
      "importScripts('" *cljs-output-file* "');"
    )]
  (str
    "var document=" (serialize *document*) ";"
    (if (empty? *closure-base-path*) single-loader multi-loader)
    "self.onmessage = function(e) {"
      ns* "." fn* ".apply();"
    "};")))

(defn full-func-name [wrk]
  (js->clj (.split (str wrk) "/")))

(defn do-some [wrk]
  (let [a (worker-body (full-func-name wrk))
        b (js/Blob. (clj->js [a]))
        w (js/Worker. (.createObjectURL js/URL b))]
    (.postMessage w nil)))
