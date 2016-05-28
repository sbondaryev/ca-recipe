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

;;66 Greatest Common Divisor
(defn f[a b] (some #(if (every? integer? %) (first %)) (for [k (range (min a b) 0 -1)] [k (/ (min a b) k) (/ (max a b) k)])))
;; wow solution
;;#(loop [n % d %2]
;;   (if (zero? d) n
;;       (recur d (mod n d))))


;;67 Prime Numbers
(defn f [k]
  (->> (range)
       (drop 2)
       (remove (fn [n] (some integer? (map #(/ n %) (range 2 n)))))
       (take k)))

;;68 Recurring Theme
[7 6 5 4 3]

;;69 Merge with a Function
(defn g [f & xs]
  (->> (apply concat xs)
       (group-by first)
       (map (fn [[k v]] [k (reduce f (map second v))]))
       (into {})))

;;70 Word Sorting
(defn g [str] (sort-by #(.toLowerCase %) (re-seq #"\w+" str)))

;;71 Rearranging Code: ->
last

;;72 Rearranging Code: ->>
apply +

;;73 Analyze a Tic-Tac-Toe Board
(defn f [xo]
  (let [d #(for [i (range 0 3)] (get-in (vec %) [i i]))
        ox (apply mapv vector xo)]
    (->> (concat xo ox [(d xo) (d (reverse xo))])
         (map set)
         (some #{#{:x} #{:o}})
         (first))))

;;74 Filter Perfect Squares
(defn f [str]
  (let [xs (map read-string (re-seq #"\d+" str))]
    (->> (map #(Math/sqrt %) xs)
         (map int)
         (map #(* % %))
         (#(filter (set %) xs))
         (clojure.string/join ","))))

;;wow solution
;;(fn [s]
;;  (clojure.string/join ","
;;                       (filter
;;                        #(-> % Integer. Math/sqrt (rem 1) zero?)
;;                        (re-seq #"\d+" s))))

;;75 Euler's Totient Function
(defn f [num]
  (let [gcd #(loop [n % d %2] (if (zero? d) n (recur d (mod n d))))]
    (count (filter #(= (gcd num %) 1) (range num)))))

;;76 Intro to Trampoline
[1 3 5 7 9 11]

;;77 Anagram Finder
(defn f[xs]
  (->> (group-by sort xs)
       (vals)
       (filter #(> (count %) 1))
       (map set)
       (set)))
;;#(->> % (group-by sort) vals (filter second) (map set) set)

;;78 Reimplement Trampoline
#(loop [res (apply % %&)]
   (if (fn? res)
     (recur (res))
     res))

;;79 Triangle Minimal Path
(letfn
    [(make-path [[node & tri] path]
       (if (seq tri)
         (->> [(map rest tri) (map drop-last tri)]
              (mapcat #(make-path % (concat path node))))
         [(concat path node)]))]
  (fn [tri]
    (->> (make-path tri [])
         (map #(apply + %))
         (apply min))))

;;80 Perfect Numbers
(defn f [n]
  (->> (range 1 n)
       (filter #(zero? (mod n %)))
       (apply +)
       (= n)))

;;81 Set Intersection
(defn f [s1 s2]
  (set (filter #(some (hash-set %) s2) s1)))
;;wow solution
;;(fn [s1 s2] (set (filter s1 s2)))


;;82 Word Chains
(letfn
    [
     (cmp [s1 s2]
       (let [d (- (count s1) (count s2))]
         (cond
           (= d 0) (= 1 (count (remove identity (map #(= %1 %2) s1 s2))))
           (= d -1) (= (filter (set (filter (set s1) s2)) s1) (seq s1))
           (= d 1) (recur s2 s1)
           :else false)))
     
     (tree [xs]
       (->>
        (for [s1 xs s2 xs] [s1 s2])
        (filter #(apply cmp %))
        (reduce #(update-in %1 [(first %2)] conj (second %2)) {})))
     
     (r1 [ks path result xs]
       (if (seq ks)
         (r1 (rest ks) path (r2 (first ks) path result xs) xs)
         result))
     
     (r2 [k path result xs]
       (cond
         ((set path) k) (conj result path)
         (xs k) (r1 (xs k) (conj path k) result xs)
         :else (conj result (conj path k))))]
  
  (defn f[st]
    (->>
     (tree st)
     (r1 st [] [])
     (distinct)
     (some #(= (count %) (count st)))
     (= true))))

;;83 A Half-Truth
#(< 1 (count(distinct %&)))
;;wow solution
;;not=

;;84 Transitive Closure
(letfn
    [(grp [xs n]
       (->>
        (filter #(= n (first %)) xs)
        (mapcat #(grp xs (second %)))
        (cons n)))
     (pair [xs]
       (map #(vector (first xs) %) (rest xs)))]
  (defn g [xs]
    (->>
     (map #(grp xs (first %)) xs)
     (reduce #(concat %1 (pair %2)) [])
     (set))))
;;wow solution
;; (fn [r]
;;   (let [r' (into r
;;                  (for [[a b] r
;;                        [c d] r :when (= b c)]
;;                    [a d]))]
;;     (if (= r r')
;;       r
;;       (recur r'))))

;;85 Power Set
(defn f [xs]
  (reduce (fn [sets x]
            (->> sets
                 (map #(conj % x))
                 (into sets)))
          #{#{}} xs))

;;86 Happy numbers
(letfn [(digits[n] (map #(- (int %) (int \0)) (str n)))]
  (defn happy? [n]
    (loop [n n seen #{}]
      (cond
        (= n 1) true
        (seen n) false
        :else (recur (reduce #(+ %1 (* %2 %2)) 0 (digits n))
                     (conj seen n))))))

;;88 Symmetric Difference
#(clojure.set/union
  (clojure.set/difference %1 %2)
  (clojure.set/difference %2 %1))

;;89 Graph Tour
(letfn
    [(next-tulpes [t graph path]
       (let [node (remove (set (last path)) t)] 
         (->> (filter #(some (set node) %) graph)
              (remove #(= % t)))))

     (visited? [path]
       (->> (frequencies path)
            (some (fn [[_ freq]] (> freq 2)))))
       

     (next-graph [t graph]
       (let [[n m] (split-with (partial not= t) graph)]
         (concat n (rest m))))

     (walk [tulpes graph path final]
       (if (or (not (seq graph)) (visited? path))
         (= (set path) (set final))
         (some true? (map #(walk
                            (next-tulpes % graph path)
                            (next-graph % graph)
                            (conj path %)
                            final)
                          tulpes))))]
  (defn f [graph]
    (boolean (walk graph graph [] graph))))
;;wow solution
;;(fn [edges]
;;  (let [
;;        maps (map #(list (hash-map (first %) (rest %))
;;                         (hash-map (last %) (butlast %))) edges)
;;        all (apply merge-with concat (flatten maps))]
;;    (or (= 1 (count edges))
;;        (every? even? (map #(count (distinct (val %))) all)))))

;;90 Cartesian Product
#(set (for [i %1 j %2] (vector i j)))

;;91 Graph Connectivity
(defn f[edges]
  (let [
        maps (map #(list (hash-map (first %) (rest %))
                         (hash-map (last %) (butlast %))) edges)
        all (apply merge-with concat (flatten maps))
        connected? (fn cn? [s e path all]
                     (cond
                       (some #{s} path) false
                       (= s e) true
                       :else (some true?
                                   (map #(cn? % e (conj path s) all)
                                        (all s)))))]
    (->>
     (for [s (keys all) e (keys all) :when (not= s e)]
       (connected? s e [] all))
     (#(conj % true))
     (every? true?))))

;; wow solution
;;(fn connected? [edges]
;;  (= (set (apply concat edges))
;;     (loop [vs #{(ffirst edges)}]
;;       (if-let [nvs (seq (for [[a b] edges :when (and (vs a) (not (vs b)))] b))]
;;         (recur (into vs nvs))
;;         vs))))

;;92 Read Roman numerals
(defn f [r]
  (let [r->a {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
    (->>
     (map r->a r)
     (partition-all 2 1)
     (map (fn[[a b]] (if (and b (< a b)) (- a) a)))
     (apply +))))
       
;;93 Partially Flatten a Sequence
(defn f
  ([col] (f col []))
  ([[fst & rst] res]
   (-> res
       (#(if (some sequential? fst) (f fst %) (conj % fst)))
       (#(if (seq rst) (f rst %) %)))))
;; wow solution
;;(fn f [xs]
;; (if (every? sequential? xs) (mapcat f xs) [xs]))

;;94 Game of Life
(letfn
    [(neighbors [size yx]
       (filter (fn [new-yx]
                 (every? #(< -1 % size) new-yx))
               (map #(vec (map + yx %))
                    [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))
     (next-generation [board yx]
       (let [cel (get-in board yx)]
         (-> (map #(get-in board %) (neighbors (count board) yx))
             (frequencies)
             (get \# 0)
             (#(cond (and (= cel \#) (< 1 % 4)) \#
                     (and (= cel \space) (= % 3)) \#
                     :defauld \space)))))]

  (defn g [board]
    (->>
     (for [y (range (count board)) x (range (count (get board y)))] [y x])
     (map #(next-generation board %))
     (partition (count board))
     (map #(apply str %)))))

;;95 To Tree, or not to Tree
(defn f [xs]
  (cond
    (nil? xs) true
    (and (sequential? xs) (= (count xs) 3)) (and (f (second xs)) (f (last xs)))
    :else false))

;;96 Beauty is Symmetry
(defn f
  ([xs] (f (second xs) (last xs)))
  ([l r]
   (cond
     (and (not (sequential? l)) (not (sequential? r))) (= l r)
     (and (sequential? l) (sequential? r)) (and (= (first l) (first r))
                                  (f (second l) (last r))
                                  (f (last l) (second r)))
     :else false)))
;; wow solution
;;#(let [t (fn t [[v l r]] [v (if r (t r)) (if l (t l))])
;;       [_ l r] %]
;;   (= l (t r)))

;;97 Pascal's Triangle
(letfn [(f [xs]
          (lazy-seq
           (if (sequential? xs)
             (let [line (map #(apply + %) (partition 2 1 (concat [0] xs [0])))]
               (cons line (f line)))
             (cons '(1) (f '(1))))))]
  #(nth (f nil) (dec %)))
;; wow solution
;;(fn [n]
;;  (nth
;;   (iterate #(map +' (concat [0] %) (concat % [0])) [1])
;;   (dec n)))

;;98 Equivalence Classes
#(set (map set (vals (group-by %1 %2))))

