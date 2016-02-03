(ns sandbox.fbuilder.partial)

(defn tax-for-state [state amount]
  (cond
    (= :ny state) (* amount 0.0645)
    (= :pa state) (* amount 0.045)))

(def ny-tax (partial tax-for-state :ny))
(def pa-tax (partial tax-for-syate :pa))
