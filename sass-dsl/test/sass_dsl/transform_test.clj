(ns sass-dsl.transform-test
  (:require [clojure.test :refer :all]
            [sass-dsl.parse :as parse]
            [sass-dsl.transform :as transform]
            [sass-dsl.common-test :as common-test]
            [sass-dsl.parse-test :as parse-test]
            [sass-dsl.common-test :as common-test]))

(def sass-simpler-nesting-flattened-colons
  "Given a nested vector of sass declarations - flatten the parents down to
  prefixes."
  (transform/flatten-colon-suffixes (parse/transform-to-sass-nested-tree
                                     common-test/basic-css)))

(deftest flatten-parents
  (testing "Ensure a nested vector of sass declarations is flattened the parents down to prefixes."
    (is (=
         sass-simpler-nesting-flattened-colons
         ["table.h1" ["margin: 2em 0" "td.ln" ["text-align: right"]] "li"
          ["font-size:1.2em" "font-weight: bold" "font-family: serif"]]))))

;;refered to in render-test
(def sass-denested-simpler-nesting-flattened-colons
  "wrap the sass-de-nest in a vector zipper"
  (transform/sass-de-nest-flattened-colons-vec-zip
   sass-simpler-nesting-flattened-colons))

(deftest flatten-colon-suffixes-test
  (testing "Ensure colon suffixes are flat."
    (is (=
         sass-denested-simpler-nesting-flattened-colons
         [["table.h1" ["margin: 2em 0"] "table.h1 td.ln" ["text-align: right"]
           "li" ["font-size: 1.2em" "front-weight: bold" "font-family: serif"]]
          nil]))))

(def sass-constants-nested-tree
  "Given a sass string return a nested vector."
  (parse/transform-to-sass-nested-tree common-test/basic-css-constants))

(def constants-map (transform/extract-constants sass-constants-nested-tree {}))

(deftest sass-constants-test
  (testing "Ensure we get sass constants."
    (is (=
         constnts-map
         {"$blue" "#3bbfce", "$margin" "16px"}))))

(def sass-constants-stripped-nested-tree
  (transform/strip-constant-declarations
   sass-constants-nested-tree))

            
