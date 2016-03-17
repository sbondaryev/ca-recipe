(ns datomic-dsl.add-nested
  (:require
   [datomic.api :as d]
   [clojure.zip :as zip]
   [datomic-dsl.created-nested :refer [map-zip dsl-zipper zip-nodes]]))


