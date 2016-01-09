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

(defn all-subvecs-from-beginning [xs]
  (map #(take % xs) (range 1 (inc (count xs)))))

(defn all-subvecs [xs]
  (for [i (range (inc (count xs))) k (range i)]
    [(drop k (take i xs)) k]))

(defn longest-match-w-beginning [left-array right-array]
  (let [all-left-chunks (all-subvecs left-array)
        all-right-chunks-from-beginning (all-subvecs-from-beginning right-array)]
    (filter #(some #{(first %)} all-right-chunks-from-beginning) all-left-chunks)))
        
