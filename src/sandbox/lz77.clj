(ns sandbox.lz77
  (:require [clojure.set :as cset]))


(defn expand [xs [distance length]]
  (->> (take-last distance xs)
       (cycle)
       (take length)))

(defn un-LZ77 [xs]
  (reduce #(if-not (vector? %2)
             (conj %1 %2)
             (into %1 (expand %1 %2)))
          [] xs))

(defn all-subvecs [xs]
  (mapcat #(partition % 1 xs) (range 1 (count xs))))
