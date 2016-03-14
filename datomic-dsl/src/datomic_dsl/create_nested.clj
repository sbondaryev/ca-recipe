(ns datomic-dsl.create-nested
  (:require
   [clojure.zip :as zip]
   [datomic.api :as d]))

(defn map-zip
  "Define a custom zipper for maps similar to zip/vector-zip and zip/seq-zip."
  [m]
  (zip/zipper
   (fn [x] (or (map? x) (map? (nth x 1))))
   (fn [x] (seq (if (map? x) x (nth x 1))))
   (fn [x children]
     (if (map? x)
       (into {} children)
       (assoc x 1 (into () children))))
   m))

(defn dsl-zipper
  "Given a dsl, return a zipper so its nested nodes can be walked with a HOF like map."
  [dsl]
  (map-zip dsl))

;;http://josf.info/blog/2014/04/14/seqs-of-clojure-zippers/
(defn zip-nodes
  "Returns all nodes in loc."
  [loc]
  (take-while (complement zip/end?) ; take until the :end
              (iterate zip/next loc)))
