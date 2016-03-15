;;lein test :only datomic-dsl.create-nested-test
(ns datomic-dsl.create-nested-test
  (:require [clojure.test :refer :all]
            [datomic-dsl.create-nested :refer :all]
            [datomic-dsl.common-test :refer :all]
            [clojure.pprint :refer :all]
            [datomic.api :as d]))
