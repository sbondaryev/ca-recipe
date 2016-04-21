(ns worker.macros)

(defmacro do-some [& body]
  `(let [~'w (js/Worker. (.createObjectURL js/URL worker.worker/worker-blob))]
    (defn ^:export worker# [] ~@body)
    (set! (.-onmessage ~'w) (fn [~'e] (js/console.log (worker.worker/*deserialize* (.-data ~'e)))))
    (let [~'wmeta (meta (var worker#))]
      (println ~'wmeta)
      (.postMessage ~'w (cljs.core/clj->js [(:ns ~'wmeta) (:name ~'wmeta)])))))
