(ns sass-dsl.parse
  (:require [clojure.zip :as zip]
            [clojure.string :refer [split] :as str]))

(defn get-lines
  "Tokenise the input into lines."
  [input-str]
  (:pre [(= (class input-str) java.lang.String)])
  (split input-str #"\n+"))

(defn count-space-indent
  "Count the number of spaces used to indent."
  [input-string]
  (count (take-while #{\space} input-string)))

(defn has-indent
  "Does this line has an indent?"
  [loc]
  (pos? (count-space-indent (zip/node loc))))

(defn drop-one-indent
  "Shuffle this down an indent by stripping two leading spaces."
  [input-string]
  (:pre [(and (= (.charAt input-string 0) \space) (= (.charAt input-string 1) \space))])
  (subs input-string 2))

(defn is-left-branch
  "Is there a branch to the left?"
  [loc]
  {:pre [(not (nil? loc))]}
  (if (zip/left loc)
    (zip/branch? (zip/left loc))))

(defn push-down-indents
  "Given a particular location in the tree - turn leading spaces into three nodes."
  [loc]
  (if (zip/end? loc)
    (zip/root loc)
    (if (has-indent loc)
      (let [reduced-indent-node (drop-one-indent (zip/node loc))]
        (if (is-left-branch loc)
          (recur (-> loc
                     zip/left
                     (zip-arround-child  reduced-indent-node)
                     zip/right
                     zip/remove))
          (recur (-> loc
                     (zip/insert-letf [])
                     zip/remove
                     (zip/append-child reduced-indent-node)))))
      (recur (zip/next loc)))))
