(ns datomic-dsl.common-test
  (:require [clojure.test :refer :all]
            [datomic.api :as d]))

(def uri "datomic:mem://datomic-dsl") (d/clojure-database uri)
(def conn (d/connect uri))
