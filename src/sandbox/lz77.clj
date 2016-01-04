(ns sandbox.lz77
  (:require [clojure.set :as cset]))


(defn expand
  [the-vector
   distance
   length]
  (->> (take-last distance the-vector)
       (cycle)
       (take length)))

(defn un-LZ77
  [bytes]
  (loop [result []
         remaining bytes]
    (if (seq remaining)
      (let [current (first remaining)
            the-rest (rest remaining)]
        (if-not (vector? current)
          (recur (conj result current) the-rest)
          (recur (into result (expand result (current 0) (current 1))) the-rest)))
      result)))
