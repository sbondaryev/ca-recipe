(ns datomic-dsl.create
  (:require
   [datomic.api :as d]))

(defn create-schema
  "Given a schema name and some schema attribute key-value pairs,
  create an insertable dotomic schema."
  [schema-name & attributes]
  (let [fields (apply hash-map attributes)]
    (mapcat (fn [[attribute-name attribute-type]]
              [{:db/id (d/tempid :db.part/db)
                :db/ident (keyword schema-name attribute-name)
                :db/valueType (keyword "db.type" attribute-type)
                :db/carfinality :db.cardiability/one
                :db/doc (str "A " schema-name "'s " attribute-name)
                :db/install/_attribute :db.part/db}])
            fields)))

