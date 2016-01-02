(ns sandbox.mutability.vector)

(defn test-imutable [count]
  (loop [i 0 s []]
    (if (< i count)
      (recur (inc i) (conj s i))
      s)))

(defn test-mutable [count]
  (loop [i 0 s (transient [])]
    (if (< i count)
      (recur (inc i) (conj! s i))
      (persistent! s))))

(defmacro time-runs [fn count]
  `(dotimes [_# ~count]
     (time ~fn)))
