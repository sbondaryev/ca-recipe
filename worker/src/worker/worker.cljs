(ns worker.worker
  (:require [goog.dom :as dom] [cljs.reader]))

(declare cljs-output-file)

(def *closure-base-path* goog/basePath)
(def *closure-base-file* (str *closure-base-path* "base.js"))
(def *serialize* pr-str)
(def *deserialize* identity)
(def *env-objs* {
  :document {:getElementsByTagName "function() {return [];}"}
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

(defn generate-obj-body [obj]
  (->> obj
    (map (fn [[key val]] (str (name key) ":" val)))
    (interpose ",")
    (#(str "{" (apply str %) "}"))))

(defn genetate-env [objs]
  (reduce
    (fn [res [obj body]] (str res "var " (name obj) "=" (generate-obj-body body) ";"))
    "" objs))

(def *cljs-output-file* (cljs-output-file))
(def *env-str* (genetate-env *env-objs*))

(defn ^:export pr-str-js [code] (*serialize* code))

(def subworker "
self.Worker = function(path){
  var that = this;
  this.id = Math.random().toString(36).substr(2, 5);

  this.eventListeners = {
    'message': []
  };
  self.addEventListener('message', function(e){
    if (e.data._from === that.id){
      var newEvent = new MessageEvent('message');
      newEvent.initMessageEvent('message', false, false, e.data.message, that, '', null, []);
      that.dispatchEvent(newEvent);
      if (that.onmessage){
        that.onmessage(newEvent);
      }
    }
  });
  self.postMessage({
    _subworker: true,
    cmd: 'newWorker',
    id: this.id,
  });
};
Worker.prototype = {
  onerror: null,
  onmessage: null,
  postMessage: function(message){
    self.postMessage({
      _subworker: true,
      id: this.id,
      cmd: 'passMessage',
      message: message
    });
  },
  terminate: function(){
    self.postMessage({
      _subworker: true,
      cmd: 'terminate',
      id: this.id
    });
  },
  addEventListener: function(type, listener, useCapture){
    if (this.eventListeners[type]){
      this.eventListeners[type].push(listener);
    }
  },
  removeEventListener: function(type, listener, useCapture){
    var index = this.eventListeners[type].indexOf(listener);
    if (index !== -1){
      this.eventListeners[type].splice(idx, 1);
    }
  },
  dispatchEvent: function(event){
    var listeners = this.eventListeners[event.type];
    for (var i = 0; i < listeners.length; i++) {
      listeners[i](event);
    }
  }
};")

(defn create-worker-body []
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
     "goog.require('worker.worker');")
    single-loader (str
      "importScripts('" *cljs-output-file* "');"
    )]
  (str
    subworker
    *env-str*
    (if (empty? *closure-base-path*) single-loader multi-loader)
    "self.onmessage = function(e) {"
      "var ns = e.data[0];"
      "var fn = e.data[1];"
      (if-not (empty? *closure-base-path*) "goog.require(ns);")
      "var res = eval(ns+'.'+fn)();"
      "self.postMessage(worker.worker.pr_str_js(res));"
    "};")))

(def worker-body (create-worker-body))
(def worker-blob (js/Blob. (clj->js [worker-body])))

(defn do-some [wmeta]
  (let [w (js/Worker. (.createObjectURL js/URL worker-blob))]
    (set! (.-onmessage w) (fn [e] (println (:prnt (*deserialize* (.-data e))))))
    (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))
    (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))
    (.postMessage w (cljs.core/clj->js [(:ns wmeta) (:name wmeta)]))
    ))
