(ns sandbox.command.cash-register)

(defn make-cash-register []
  (let [register (atom 0)]
    (set-validator! register (fn [new-total] (>= new-total 0)))
    register))

(defn add-cash [register to-add]
  (swap! register + to-add))

(defn reset [register]
  (swap! register (fn [oldval] 0)))

(defn make-purchase [register amount]
  (fn []
    (println (str "Purchase in amount: " amount))
    (add-cash register amount)))

(def purchases (atom []))

(defn execute-purchase [purchase]
  (swap! purchases conj purchase)
  (purchase))

;;test
(def register (make-cash-register))
(def purchase-1 (make-purchase register 100))
(def purchase-2 (make-purchase register 50))
(execute-purchase purchase-1)
(execute-purchase purchase-2)
(reset register)
(doseq [purchase @purchases] (purchase))
