(ns sandbox.fbuilder.selector)

(defn selector [& path]
  {:pre [(not (empty? path))]}
  (fn [ds] (get-in ds path)))

(def person {:name "Some Name"})
(def morePerson {:address {:street {:name "Fake St."}}})
   
(def personName (selector :name))
(def streetName (selector :address :street :name))
