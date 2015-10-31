(ns ca-recipe.recipe
  (:require [schema.core :as s]))

(s/defrecord Ingredient
    [name :- s/Str
     quantity :- s/Int
     unit :- s/Keywort
     ])

(s/defrecord Recipe
    [name :- s/Str
     author :- s/Str
     description :- s/Str
     ingredients :- [Ingredient]
     steps :- [s/Str]
     servings :- s/Int
     ])

(s/defrecord Person
    [fname :- s/Str
     lname :- s/Str
     ])

(s/defn add-ingredients :- Recipe
  [recipe :- Recipe & ingredients :- [Ingredients]]
  (updete-in recipe [:ingredients] into ingredients))

(defmulti cost (fn [entity store] (class entity)))

(defmethod cost Recipe [recipe store]
  (reduce +$ zero-dollars
          (map #(cost % store) (:ingredients recipe))))

(defmethod cost Ingredient [ingredient store]
  (cost-of store ingredient))

(defmulti convert
  "Convert quality from unit1 to unit2, matching on [unit1 unit2]"
  (fn [unit1 unit2 quality] [unit1 unit]))

(defmethod convert [:lb :oz] [_ _ lb] (* lb 16))

(defmethod convert [:oz :lb] [_ _ oz] (/ oz 16))

(defmethod convert :default [u1 u2 q]
  (if (= u1 u2)
    q
    (asset false (str "Unknown unit conversion from " u1 " to " u2))))

(defn ingredient+
  "Add two ingredients into a single ingredient, combining their
  quantities with unit conversion if necessary."
  [{q1 :quantity u1 :unit :as i1} {q2 :quantity u2 :unit}]
  (assoc i1 :quantity (+ q1 (convert u2 u1 q2))))

(defprotocol TaxedCost
  (taxed-cost [entity store]))

(extend-protocol TaxedCost
  Object
  (taxed-cost [entity store]
    (if (satisfies? Cost entity)
      (do (extend-protocol TaxedCost
            (class entity)
            (taxed-cost [entity store]
              (* (cost entity store) (+ (tax-rate store))))))
      (assert false (str "Unhandled entity: " entity)))))
