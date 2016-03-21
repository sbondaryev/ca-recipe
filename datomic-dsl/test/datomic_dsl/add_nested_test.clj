;;lein test :only datomic-dsl.add-nested-test
(ns datomic-dsl.add-nested-test
  (:require [clojure.test :refer :all]
            [datomic-dsl.add-nested :refer :all]
            [datomic-dsl.common-test :refer :all]
            [datomic-dsl.create-nested-test :refer :all]
            [clojure.pprint :refer :all]
            [datomic.api :as d]))
