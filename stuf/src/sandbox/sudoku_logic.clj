(ns sandbox.sudoku-logic
  (require [clojure.core.logic :as logic]))

(def b1 '[3 - - - - 5 - 1 -
          - 7 - - - 6 - 3 -
          1 - - - 9 - - - -
          7 - 8 - - - - 9 -
          9 - - 4 - 8 - - 2
          - 6 - - - - 5 - 1
          - - - - 4 - - - 6
          - 4 - 7 - - - 2 -
          - 2 - 6 - - - - 3])

(defn rowify [board]
  (->> board
       (partition 9)
       (map vec)
       vec))

(defn colify [rows]
  (apply map vector rows))

(defn subgrid [rows]
  (partition 9
             (for [row (range 0 9 3)
                   col (range 0 9 3)
                   x (range row (+ row 3))
                   y (range col (+ col 3))]
               (get-in rows [x y]))))
