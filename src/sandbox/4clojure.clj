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

;;44 Rotate Sequence
#(->> %2
      (split-at (mod %1 (count %2)))
      (reverse)
      (apply concat))

;;45 Intro to Iterate
[1 4 7 10 13]

;;46 Flipping out
#(fn [& args] (apply % (reverse args)))

;;47 Contain Yourself
4

;;48 Intro to some
6

;;49 Split a sequence
#(conj [] (take %1 %2) (drop %1 %2))

;;50 Split by Type
#(vals (group-by type %))

;;51 Advanced Destructuring
[1 2 3 4 5]

;;52 Intro to Destructuring
;;[c e]

;;53 Longest Increasing Sub-Seq
(fn [col]
  (->> (map #(conj [] %1 %2) (drop-last col) (rest col))
       (partition-by (fn [[a b]] (not= (- b a) 1)))
       (filter (fn [sub] (some (fn [[a b]] (= (- b a) 1)) sub)))
       (sort-by count)
       (last)
       (apply concat)
       (distinct)))
;; wow solution
;;(fn [xs]
;;  (->> xs
;;       (map #(vector % %2) (range))
;;       (partition-by #(- (last %) (first %)))
;;       reverse
;;       (apply max-key count)
;;       (map last)
;;       (#(if (< (count %) 2) [] %))))

;;54 Partition a Sequence
#(loop [xs %2  res []]
   (if (>= (count xs) % )
     (recur (drop % xs) (conj res (take % xs)))
     res))

;;55 Count Occurrences
(fn [xs] (reduce-kv #(assoc % %2 (count %3)) {} (group-by identity xs)))

;;56 Find Distinct Items
(fn [xs] (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] xs))

;;57 Simple Recursion
[5 4 3 2 1]

;;58 Function Composition
(fn f [& func]
  (fn g [& param]
    (first (reduce #(conj [] (apply %2 %1)) param (reverse func)))))
;; wow solution
;; (fn [& args] (reduce (fn [a b] (fn [& more] (a (apply b more)))) args))

;;59 Juxtaposition
(fn [& fs] (fn [& args] (map #(apply % args) fs)))

;;60 Sequence Reductions
(fn f
  ([op a] (f op (first a) (rest a)))
  ([op a b]
   (if (seq b)
     (lazy-seq (cons a (f op (op a (first b)) (rest b))))
     [a])))

;;61 Map Construction
(fn [a b] (into {} (map #(conj [] %1 %2) a b)))

;;62 Re-implement Iterate
(fn g [f x]
  (lazy-seq (cons x (g f (f x)))))

;;63 Group a Sequence
(fn [f xs] (reduce #(update %1 (f %2) (comp vec conj) %2) {} xs))

;;64 Intro to Reduce
+

;;65 Black Box Testing
(fn f [xs]
  (case (conj (empty xs) [:a 1] [:a 2])
    {:a 2} :map
    [[:a 1] [:a 2]] :vector
    [[:a 2] [:a 1]] :list
    #{[:a 2] [:a 1]} :set))
    
