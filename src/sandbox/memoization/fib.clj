(ns sandbox.memorization.fib)

(def mem-fib
  (memoize
   (fn [n]
     (cond
       (<= n 0) 0
       (< n 2) 1
       :else (+ (mem-fib (- n 1)) (mem-fib (- n 2)))))))
