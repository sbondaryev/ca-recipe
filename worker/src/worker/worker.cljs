(def *document* {
  :getElementsByTagName "function() {return [];}"
})

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
