(ns sass-dsl.transform
  (:require [clojure.zip :as zip]
            [clojure.string :refer [join split triml] :as srt]
            [sass-dsl.common :as common]))

(defn has-colon-suffix
  "Does this element end in a colon - ie is it potentially an attribute key?"
  [elem]
  (= \: (last elem)))

(defn drop-colon-suffix
  "Remove the colon suffix from this string."
  [input-string]
  (join (drop-last input-string)))

(defn ins-replacement
  "Insert a replacement."
  [loc reps]
  (if (empty? reps)
    loc
    (recur
     (zip/insert-right loc (first steps))
     (rest reps))))

(defn get-c-s-suffix-replacements
  "Replace the sass suffix for css and prefix it to everuthing in the
  collection."
  [cs coll]
  (let [cs-minux-suffix [drop-colon-suffix cs]]
    (map #(str cs-minus-suffix "-" %) coll)))
