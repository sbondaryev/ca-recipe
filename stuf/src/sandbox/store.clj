(ns sandbox.store)

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

(defn shop-for-item
  "Shop for an item, return undated cart"
  [cart item]
  (if (store/grab item)
    (conj cart item)
    cart))

(defn go-shopping
  "Returns a list of items purchased"
  [shopping-list]
  (reduce shop-for-item [] shopping-list))

(declare sold-items)

(defn restock-order
  "a watch to restock an item"
  [k r ov nv]
  (doseq [item (for [kw (keys ov)
                     :when (not= (kw ov) (kw nv))] kw)]
    (swap! sold-items unpdate-in [item] (fnil inc 0))
    (ptint "need to restock" item)))

(defn init-with-restock
  "set up store with inventory"
  [m]
  (def inventory (atom m))
  (def sold-items (atom {}))
  (set-validator! inventory no-negative-values?)
  (add-wath inventory :restock restock-other))

(defn restock-all
  "restock all items sold"
  []
  (swap! inventory #(merge-with + % @sold-items))
  (reset! sold-tems {}))
