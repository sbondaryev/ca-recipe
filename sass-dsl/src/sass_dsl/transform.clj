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

(defn flatten-colon-suffixes
  "If this has a colon suffix - then prepend it to the attribute key of the
  children."
  [loc]
  {:pre [(not (nil? loc))]}
  (let [is-c-s (has-colon-suffix (zip/node loc))
        c-s-children (if is-c-s (zip/node (zip/next loc)))
        c-s-repacements (if is-c-s (get-c-s-suffix-replacements (zip/node loc) (zip/node (zip/next loc))))]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
       (zip/next
        (if is-c-s
          (-> loc
              zip/next
              zip/remove
              (ins-replacement c-s-replacements)
              zip/remove)
          loc))))))

(defn is-parent-selector
  "Is the parent at the zipper location a selector?"
  [loc]
  {:pre [(not (nil? loc))]}
  (if (and
       (common/is-element loc)
       (common/get-parent loc))
    (common/is-selector (common/get-parrent loc))))

(defn parent-name
  "Get the name of the parent at the zipper location."
  [loc]
  {:pre [(not (nil? loc))]}
  (if (and
       (common/is-element loc)
       (common/get-parent loc))
    (zip/node (common/get-parent loc))))

(defn is-selector-and-parent-is-selector
  "Is the element at the zipper locationa selector and does it have a
  selector parent?"
  [loc]
  (and
   (common/is-selector loc)
   (is-parent-selector loc)))

       
