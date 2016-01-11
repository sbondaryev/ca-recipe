(ns sandbox.fractal
  (:import (java.awt image.BufferedImage Color)
           (javax.imageio ImageIO)
           (java.io File)))

(defn pascal-row-step [yield pascal-row]
;;=> pascal-row is the one above the row we're computing
  {:pre [(> (get  pascal-row 0) 0)]}  ;=> We can only start from [1]!
  (let [cnt-elts (count pascal-row)
        half-row (subvec pascal-row 0 (inc (double (/ cnt-elts 2))))
;;=> We compute half the above row
        padded-half-row (into [0] half-row)
;;=> and add a 0 to the beginning, as we'll use it in computation
        half-step (vec (map (comp (partial apply yield) vec)
                            (partition 2 1 padded-half-row)))
;;=> we compute the first half, summing the above element
;;      and the element at the left of the above one.
        other-half-step (vec (if (even? cnt-elts)
                               (-> half-step
                                   butlast
                                   reverse)
                               (-> half-step
                                   reverse)))]
;;=> the mirror of the half we computed. If count elements is
;; even, we omit the last element from half-step.
    (into half-step other-half-step)))
;;=> we return half to which we append the mirror copy.
