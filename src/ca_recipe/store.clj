(ns ca-recipe.store)

(defn go-shopping-naive
  "Returns a list of items purchased" [shopping-list]
  (loop [[item & items] shopping-list
         cart []] (if item
                    (recur items (conj cart item)) cart)))
