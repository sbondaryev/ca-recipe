(ns sandbox.deriv)

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2] (list '+ a1 a2))
(defn make-product [a1 a2] (list '* a1 a2))

(defn sum? [[x :as s]]
  (and (seq s) (= x '+)))
(defn addend [[_ x]] x)
(defn augend [[_ _ x]] x)

(defn product? [[x :as p]]
  (and (seq p) (= x '*)))
(defn multiplier [[_ p]] p)
(defn multiplicand [[_ _ p]] p)

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp)
         (if (same-variable? exp var) 1 0)
        (sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
        (product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
        :else
         (println "unknown expresion type -- DERIV" exp)))
