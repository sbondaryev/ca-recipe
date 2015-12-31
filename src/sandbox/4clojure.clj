(ns sandbox.4clojure)

;;1 Nothing but the Truth
true

;;2 Simple Math
4

;;3 Intro to Strings
"HELLO WORLD"

;;4 Intro to Lists
:a :b :c

;;5 Lists: conj
'(1 2 3 4)

;;6 Intro to Vectors
:a :b :c

;;7 Vectors: conj
[1 2 3 4]

;;8 Intro to Sets
#{:a :b :c :d}

;;9 Sets: conj
2

;;10 Intro to Maps
20

;;11 Maps: conj
[:b 2]

;;12 Intro to Sequences
3

;;13 Sequences: rest
[20 30 40]

;;14 Intro to Functions
8

;;15 Double Down
(partial * 2)


;;16 Hello World
#(str "Hello, " % "!")

;;17 Sequences: map
'(6 7 8)

;;18 Sequences: filter
[6 7]

;;19 Last Element
#(if (empty? (rest %)) (first %) (recur (rest %)))

;;20 Penultimate Element
#(if (empty? (rest(rest %))) (first %) (recur (rest %)))


;;21 Nth Element
#(cond
   (= %2 0) (first %1)
   (or (< %2 0) (empty? (rest %1))) nil
   :else (recur (rest %1) (dec %2)))

;;22 Count a Sequence
#(loop [col % nbr 1]
   (if (empty? (rest col))
     nbr
     (recur (rest col) (inc nbr))))

;;23 Reverse a Sequence
#(loop [col % rcol '()]
   (if (empty? col)
     rcol
     (recur (rest col) (conj rcol (first col)))))

;;24 Sum It All Up
reduce +

;;25 Find the odd numbers
filter odd?

;;26 Fibonacci Sequence
(fn [nbr]
  (->> (iterate (fn[[a b]] [b (+ a b)]) [1 1])
       (map first)
       (take nbr)))

;;27 Palindrome Detector
#(= (seq %) (reverse %))

;;28 Flatten a Sequence
(partial (fn flat[res col]
           (let [fst (first col) rst (rest col)]
             (-> res
                 (#(if (sequential? fst) (flat % fst) (conj % fst)))
                 (#(if (empty? rst) % (flat % rst)))))) [])

;;29 Get the Caps
(fn [s] (apply str (filter #(Character/isUpperCase %) s)))

;;30 Compress a Sequence
reduce #(if-not (= (last %1) %2) (conj %1 %2) %1) []

;;31 Pack a Sequence
partition-by identity

;;32 Duplicate a Sequence
#(interleave % %)

;;33 Replicate a Sequence
(fn [col n] (reduce #(concat %1 (repeat n %2)) [] col))

;;34 Implement range
#(loop [a %1 col []]
   (if (>= a %2)
     col
     (recur (inc a) (conj col a))))

;;35 Local bindings
7

;;36 Let it Be
;;[z 1 y 3 x 7]

;;37 Regular Expressions
"ABC"

;;38 Maximum value
(comp last sort (partial conj '()))

;;39 Interleave Two Seqs
mapcat list

;;40 Interpose a Seq
(fn [s, col] (drop-last (mapcat #(list % s) col)))

;;41 Drop Every Nth Item
#(mapcat (partial take (dec %2)) (partition-all %2 %1))

;;42 Factorial Fun
#(apply * (range 1 (inc %)))

;;43 Reverse Interleave
#(loop [col (partition %2 %1) res []]
   (if-not (every? seq  col)
     res
     (recur (map rest col) (conj res (map first col)))))
