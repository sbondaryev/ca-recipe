(ns sandbox.eight-queens)

(letfn
    [(queen-cols [k]
       (if (= k 0)
         (list empty-board)
         (filter
          (fn [positions] (safe? k positions))
          (mapcat
           (fn [rest-of-queens]
             (map (fn [new-row]
                    (adjoin-position new-row k rest-of-queens))
                  (interval 1 board-size)))
           (queen-cols (- k 1))))))]
  (defn queens [board-size]
    (queen-cols board-size)))


