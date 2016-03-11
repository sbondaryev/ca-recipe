(ns datomic-dsl.add
  (:require
   [datomic.api :as d]))

(defn convert-dsl-to-keywords-fn
  "Pass this in as a  parameter so we can use it later."
  [schema-name]
  (fn [[attribute-key attribute-value]]
    ;;Build a vector for the datomic fact add, if the value is a map then we'll set ID
    [(keyword schema-name attribute-key) attribute-value]))

;;TODO: why concat?
(defn convert-to-map
  "Format the input vector for the Datomic map syntax."
  [input]
  [(apply hash-map (first (concat input)))])

(defn add-datom
  "Given a schema name and some key-value pairs, generate a Datomic insert string. Nested values are out of scope."
  [schema-name attributes]
  (let [convert-dsl-to-keywords (convert-dsl-to-keywords-fn schema-name)]
    (convert-to-map
     [(concat [:db/id (d/tempid :db.part/user)]
              (mapcat convert-dsl-to-keywords attributes))])))
