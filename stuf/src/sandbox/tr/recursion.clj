(ns sandbox.tr.recursion)

(defn make-people [first-names last-names]
  (loop [first-names first-names last-names last-names people []]
    (if (seq first-names)
      (recur
       (rest first-names)
       (rest last-names)
       (conj people
             {:first (first first-names) :last (first last-names)}))
      people)))



(def first-names ["1" "2" "3"])
(def last-names ["A" "B" "C"])
