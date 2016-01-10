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
  (let [left-chunks (all-subvecs left-array)
        right-chunks (all-subvecs-from-beginning right-array)]
    (->>(filter #(some #{(first %)} right-chunks) left-chunks)
        (sort-by #(count (first %)))
        (last))))
        
(defn LZ77-STEP [[window look-ahead]]
  (if-let [longest (longest-match-w-beginning window look-ahead)]
    {:length (count (first longest))
     :char (conj [] (- (count window) (second longest)) (count (first longest)))}
    {:length 1
     :char (first look-ahead)}))

(defn slider [window xs]
  (map #(conj [] (take-last window (take % xs)) (drop % xs)) (range 0 (count xs))))

(defn compress [chunks]
  (loop [chunks chunks res []]
    (if (seq chunks)
      (let [rc (LZ77-STEP (first chunks))]
        (recur (drop (:length rc) chunks) (conj res rc)))
      res)))

(defn LZ77 [bytes-array window-size]
  (->> (slider window-size bytes-array)
       (compress)
       (map :char)))
