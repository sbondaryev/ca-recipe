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
