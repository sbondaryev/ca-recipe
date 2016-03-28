;;lein test :only datomic-dsl.add-nested-test
(ns datomic-dsl.add-nested-test
  (:require [clojure.test :refer :all]
            [datomic-dsl.add-nested :refer :all]
            [datomic-dsl.common-test :refer :all]
            [datomic-dsl.create-nested-test :refer :all]
            [clojure.pprint :refer :all]
            [datomic.api :as d]))

(def nest-datom-dsl
  "Define data structure."
  {"borrowevent"
   {"book" {"title" "Mutiny on the Bounty"
            "author" "Charles Nordoff"}
    "borrower" {"name" "John Smith"}
    "date" "today"}})

(def nested-datomic-add-syntax
  "Convert data structure to datomic syntax."
  (convert-dsl-to-datomic-syntax "borrowevent" nest-datom-dsl))

;;(pprint nested-datomic-add-syntax)

(def add-result
  "Keep a reference to the transaction result"
  (d/transact conn nested-datomic-add-syntax))

(def borrower-datom
  "Query against the result"
  (d/touch
   (d/entity
    (d/db conn)
    (d/q '[:find ?e . :where [?e :borrower/name]]
         (d/db conn)))))

;;(pprint borrowed-datom)

(deftest datom-added-test
  (testing "Is the datom added in the database?"
    (is (= 1 (count borrower-datom)))))
