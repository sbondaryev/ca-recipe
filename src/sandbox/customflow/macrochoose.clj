(ns sandbox.customflow.macrochoose)

(defmacro simpler-choose [num first second third]
  `(cond
     (= 1 ~num) ~first
     (= 2 ~num) ~second
     (= 3 ~num) ~third))
