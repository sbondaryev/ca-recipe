(ns worker.macros)

(defmacro code->str [& body]
  `(worker.utils/worker-body
    (worker.utils/cljs->js '(do ~@body))))
