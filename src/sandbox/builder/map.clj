(ns sandbox.builder.map)

(def p
  {:first-name "John"
   :middle-name "Quincy"
   :last-name "Adams"})

(into {} (for [[k, v] p] [k (.toUpperCase v)]))
