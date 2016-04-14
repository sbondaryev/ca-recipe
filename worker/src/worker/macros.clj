(ns worker.macros)

(defmacro code->str [ns* & body]
  `(let [ns# ~ns*]
    (worker.utils/worker-body ns#
    (worker.utils/cljs->js ns# '(do ~@body) ))))
