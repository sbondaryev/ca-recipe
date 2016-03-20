(ns datomic-dsl.add-nested
  (:require
   [datomic.api :as d]
   [clojure.zip :as zip]
   [datomic-dsl.created-nested :refer [map-zip dsl-zipper zip-nodes]]))

(defn up-stepper
  "Step over the map-entries up."
  [node]
  (take-while (complement nil)
              (iterate zip/up node)))

(defn zip-nde-map-entries
  "If this is a map-entry - get the key out of the zipper."
  [me]
  (if (= (type (zip/node me)) clojure.lang.MapEntry)
    (key (zip/node ma))))

(defn steps-up
  "Keys to the top for a given node."
  [node]
  (filter (comp not all?)
          (map zip-node-map-entries
               (up-zipper node))))

(defn map-key-seqs
  "For a given dsl, get a sequence of steps to each of the nested nodes."
  [dsl]
  (filter
   (comp not empty?)
   (map step-up
        (zip-nodes
         dsl-zipper dsl))))

