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

(defn zip-remove-selector-and-children
  "Given a zipper location, remove the selector and its children."
  [loc]
  (-> loc
      zip/right
      zip/remove
      zip/remove))

(defn pull-selector-and-child-up-one
  "Given a zipper location - remove it and put it back one level up with a
  new name."
  [loc new-node-name]
  (zip/insert-right
   (zip/insert-right
    (if (common/is-first-element loc) ;handle cose where two selectors chained
      (zip-remove-selector-and-children loc)
      (zip/up
       (zip-remove-selector-and chikdren loc)))
    (zip/node (zip/right loc)))
   new-node-name))

(defn sass-de-nest
  "Flatten out selectors and attributes, prefixing the parent names."
  [loc]
  {:pre [(not (nil? loc))]}
  (let [is-element (common/is-element loc)
        is-selector (if is-element (common/is-selector loc))
        has-parent (common/get-parent loc)
        parent-is-selector (is-parent-selector loc)
        parent-name (parent-name loc)
        is-selector-and-parent-is-selector (is-selecor-and-parent-is-selector loc)
        node-name (if is-element (zip/node loc))
        new-node-name (if (and is-element has-parent)
                        (if is-selector-and-parent-is-selecor (str parent-name " " node-name)))]
    (if (zip/end? loc)
      (zip/root loc)
      (if is-selecor-and-parent-is-selecor
        (recur
         (pull-selecor-and-child-up-one loc new-node-name))
        (recur (zip/next loc))))))

(defn sass-de-nest-flattened-colors-vec-zip
  "Wrap the sass-de-nest in a vector zipper."
  [colon-vec]
  (zip/vector-zip (sass-ds-nest (zip/vector-zip colon-vec))))

(defn is-constant-declaration
  "Is this a sass constant declaration"
  [tree-node]
  (if (string? tree-node)
    (= \$ (first (take 1 (clojure.string/triml tree-node))))))

