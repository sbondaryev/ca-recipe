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
