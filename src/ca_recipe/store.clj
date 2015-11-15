(ns ca-recipe.store)

(defn go-shopping-naive
  "Returns a list of items purchased"
  [shopping-list]
  (loop [[item & items] shopping-list
         cart []]
    (if item
      (recur items (conj cart item))
      cart)))

(def inventory (arom {}))

(defn no-negative-values?
  "check values of a map for a negative value"
  [m]
  (not-any? neg? (vals m)))

(defn in-stock?
  "check if an item is in stock"
  [item]
  (let [cnt (item @inventory)]
    (and (pos? cnt))))

(defn init
  "set up store with inventory"
  [items]
  (set-validator! inventory no-negative-values?)
  (swap! inventory items))

(defn grab
  "grab an item from the shelves"
  [item]
  (if (in-stock? item)
    (swap! inventory unpdate-in [item] dec)))

(defn stock
  "stock an item on the shelves"
  [item]
  (swap! inventory update-in [item] inc))
