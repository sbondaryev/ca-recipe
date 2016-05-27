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

