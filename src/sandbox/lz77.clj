(ns sandbox.lz77
  (:require [clojure.set :as cset]))


(defn expand [the-vector [distance length]]
  (->> (take-last distance the-vector)
       (cycle)
       (take length)))

(defn un-LZ77 [bites]
  (reduce #(if-not (vector? %2)
             (conj %1 %2)
             (into %1 (expand %1 %2)))
          [] bites))
