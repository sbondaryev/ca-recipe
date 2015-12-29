(ns sandbox.fmr.discount)

(defn calculate-discount [prices]
  (reduce +
          (map (fn [price] (* price 0.10))
               (filter (fn [price] (>= price 20.0)) prices))))
