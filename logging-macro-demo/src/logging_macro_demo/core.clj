(ns logging-macro-demo.core
  (:require [clojure.math.numeric-tower :as math]
            [clojurewerkz.money.amounts :refer [amount-of]]
            [clojurewerkz.money.currencies :refer [USD]]
            [clojurewerkz.money.format :as mf])
  (:gen-class))

(defn round
  "Given value and a precision return a rounded number."
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/floor (* d factor)) factor)))

(defn format-currency
  "Do decimal currency formatting using the
  Clojurewerkz/Joda currency library."
  [input] (mf/format (amount-of USD (round input 2))))

(def balance [3367.01 7839.76 326478.01 23479.15])

(defn indices
  "Given a predicate function and a collection, return a list of collection
  indexes that satisfy the predicate."
  [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
