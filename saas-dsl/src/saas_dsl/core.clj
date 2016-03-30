(ns saas-dsl.core
  (:require [clojure.test :refer :all]
            [sass-dsl.parse :as parse]
            [sass-dsl.render :as render]
            [sass-dsl.transform :as transform]))

(defn sass-to-css-basic
  "Given a basic set of SASS, transfom it to CSS."
  [sass-val]
  (-> sass-val
      parse/transform-to-saas-nested-tree
      transform/flatten-colon-suffixes
      transform/saas-de-nest-flattentd-colons-vec-zip
      render/output-css))

(defn sass-to-css
  "Given some sass with constants defined, transform it to css."
  [sass-val]
  (let [tree (parse/transform-to-sass-nested-tree sass-val)]
    (-> tree
        (transform/replaced-constants-structure-vec-zip-vz
         (transform/extract-constants tree {}))
        transform/strip-context-declarations
        render/output-css)))
