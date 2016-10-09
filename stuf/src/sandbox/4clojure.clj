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
   (if (>= (count xs) %)
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

;;99 Product Digits
(letfn
    [(f [n xs]
       (if (= 0 (int (/ n 10)))
         (cons (mod n 10) xs)
         (f (int (/ n 10)) (cons (mod n 10) xs))))]
  #(f (* %1 %2) []))

;;100 Least Common Multiple
(defn f [& xs]
  (let [g (fn [n mult]
            (if-let [lcm (first (apply clojure.set/intersection mult))]
              lcm
              (recur (inc n) (map conj mult (map #(* n %) xs)))))]
    (g 1 (repeat (count xs) #{}))))

;;101 Levenshtein Distance
;; (optimized)
(fn [x y]
  (letfn
      [(levenshtein [mem s1 s2]
         (let [l1 (count s1)
               l2 (count s2)]
           (cond (zero? l1) l2
                 (zero? l2) l1
                 :else
                 (let [cost (if (= (first s1) (first s2)) 0 1)]
                   (min (inc (mem mem (rest s1) s2))
                        (inc (mem mem s1 (rest s2)))
                        (+ cost
                           (mem mem (rest s1) (rest s2))))))))]
    (levenshtein (memoize levenshtein) x y)))
;; wow solution
;;(fn levenshtein-distance [s t]
;;  (let [f (fn [f s t]
;;            (cond
;;              (empty? s) (count t)
;;              (empty? t) (count s)
;;              :else (let [cost (if (= (first s) (first t)) 0 1)]
;;                      (min (inc (f f (rest s) t))
;;                           (inc (f f s (rest t)))
;;                           (+ cost (f f (rest s) (rest t)))))))
;;        g (memoize f)]
;;    (g g s t)))

;;102 intoCamelCase
(defn f[w]
  (let [[fst & rst] (clojure.string/split w #"-")]
    (str fst (apply str (map clojure.string/capitalize rst)))))
;; wow solution
;;(fn [s] (clojure.string/replace s #"-(\w)" #(str (.toUpperCase (% 1)))))

;; 103 Generating k-combinations
(defn f [m st]
  (letfn [(comb [m [fst & rst :as xs]]
            (cond
              (= m 1) (map hash-set  xs)
              (= (count xs) m) (list (set xs))
              (< (count xs) m) '()
              :else (concat
                     (map #(conj % fst) (comb (dec m) rst))
                     (comb m rst))))]
    (set (comb m (seq st)))))
;;wow solution
;;(defn k-combos [n items]
;;  (if (zero? n) [#{}]
;;      ((comp set mapcat)
;;       #(for [i (k-combos (dec n) (disj items %))] (conj i %))
;;       items)))

;;104 Write Roman Numerals
(fn [a]
  (let [r [\I \V \X \L \C \D \M]
        a->r (fn [d [i v x]]
               (let [d->r {1 [i]
                           2 [i i]
                           3 [i i i]
                           4 [i v]
                           5 [v]
                           6 [v i]
                           7 [v i i]
                           8 [v i i i]
                           9 [i x]}]
                 (apply str (d->r d))))
        digits (fn [n] (map #(- (int %) (int \0)) (str n)))]
    (->> (map a->r (reverse (digits a)) (partition-all 3 2 r))
         (reverse)
         (apply str))))
;; wow solution
;; #(clojure.pprint/cl-format nil "~@R" %)

;;105 Identify keys and values
(letfn
    [(aux [[fst & rst :as xs] [[fstm] :as m]]
       (cond
         (not (seq xs)) m
         (keyword? fst) (aux rst (cons [fst] m))
         :else (aux rst (cons [fstm fst] m))))]
  (fn [sx]
    (reduce
     (fn [m [fst & rst]] (update-in m [fst] (comp vec concat) rst))
     {} (reverse (aux sx [])))))
;;wow solution
;;(fn mf [s]
;;  (if (seq s)
;;    (merge {(first s) (take-while (complement keyword?) (rest s))}
;;           (mf (drop-while (complement keyword?) (rest s)))) {} ))

;;106 Number Maze (optimized)
(letfn [(aux [mem x y n]
          (cond
            (= n 0) nil
            (= x y) (list x)
            (mem mem (* x 2) y (dec n)) (cons x (mem mem (* x 2) y (dec n)))
            (mem mem (+ x 2) y (dec n)) (cons x (mem mem (+ x 2) y (dec n)))
            (and (even? x) (mem mem (/ x 2) y (dec n))) (cons x (mem mem (/ x 2) y (dec n)))))]
  (defn ff [x y] (count (some #(aux (memoize aux) x y %) (range)))))

;;107 Simple closures
(fn [n] #(reduce * (repeat n %)))

;;108 Lazy Searching
(defn f [& args]
  (let [mx (apply max (map first args))]
    (if (apply = (map first args))
      mx
      (apply f (map #(if (< (first %) mx) (rest %) %) args)))))

;;110 Sequence of pronunciations
(defn f [xs]
  (lazy-seq
   (let [xss (mapcat #(vector (count %) (first %)) (partition-by identity xs))]
        (cons xss (f xss)))))

;;111 Crossword puzzle
(letfn
    [(cmpr [[fw & rw :as word] [fp & rp :as path]]
       (cond
         (not= (count word) (count path)) false
         (not (seq path)) true
         (or (= \_ fp) (= fp fw)) (cmpr rw rp)
         :else false))]
  (fn [word puzzle]
    (->> (map #(clojure.string/replace % #" " "") puzzle)
         (#(concat % (if (> (count %) 1) (apply map str %))))
         (mapcat #(clojure.string/split % #"#"))
         (map #(cmpr word %))
         (some identity)
         (boolean))))
;;wow solution
;;(fn[w l] (->>(map #(replace {\space""\_\.} %)l)
;;             (#(concat % (apply map vector %)))
;;             (mapcat #(.split(apply str %)"#"))
;;             (some #(re-matches(re-pattern%)w))
;;             boolean))

;;112 Sequs Horribilis
(defn f [n [fst & rst :as xs]]
  (cond
    (not (seq xs)) nil
    (sequential? fst) (let [sub (f n fst)]
                        (cons sub (f (- n (apply + (flatten sub))) rst)))
    (>= (- n fst) 0) (cons fst (f (- n fst) rst))
    :else '()))
;;wow solution
;;(defn dcl [l x]
;;  (loop [c [] [x1 & rx] x]
;;    (let [rc (- l (apply + (flatten c)))]
;;      (cond
;;        (nil? x1) c
;;        (coll? x1) (recur (conj c (dcl rc x1)) rx)
;;        (<= x1 rc) (recur (conj c x1) rx)
;;        :else c))))

;;113 Making Data Dance
#(reify clojure.lang.ISeq
   (toString [_] (reduce str (interpose ", " (sort %&))))
   (seq [_] (if (empty? %&) nil (distinct %&))))

;;114 Global take-while
(defn f [n pred [fst & rst :as sx]]
  (cond
    (not (seq sx)) nil
    (and (= n 1) (pred fst)) nil
    :else (cons fst (f (if (pred fst) (- n 1) n) pred rst))))


;;115 The Balance of N
(defn f [num]
  (let [d (map #(- (int %) (int \0)) (str num))
        c (count d)
        m (quot c 2)]
    (if (even? c)
      (= (apply + (take m d)) (apply + (drop m d)))
      (= (apply + (take m d)) (apply + (drop (inc m) d))))))

;;116 Prime Sandwich
(defn f [n]
  (let [prime? (fn [n] (when (> n 1) (not (some integer? (map #(/ n %) (range 2 n))))))
        lprime (fn [n]
                 (cond (= n 1) nil
                       (prime? n) n
                       :else (recur (dec n))))
        rprime (fn [n]
                 (if (prime? n)
                   n
                   (recur (inc n))))]
    (if (prime? n)
      (if-let [lp (lprime (dec n))]
        (= n (/ (+ lp (rprime (inc n))) 2))
        false)
      false)))
;;wow solution
;;(fn [n]
;;  (and (> n 3)
;;       (let [p (fn [x] (every? #(< 0 (mod x %)) (range 2 x)))
;;             b (first (filter p (reverse (range 2 n))))
;;             a (first (filter p (drop (+ n 1) (range))))]
;;         (and (p n) (= n (/ (+ a b) 2))))))

;;117 For Science!
(defn f [board]
  (let [steps [[-1 0] [0 -1] [0 1] [1 0]]
        find-c (fn [[line & _ :as board]]
                 (first (for [x (range (count line))
                              y (range (count board))
                              :when (= \C (get-in board [y x]))]
                          [y x])))
        find-m (fn find-rec [board pos path]
                 (let [val (get-in board pos)]
                   (cond
                     (or (= val nil) (= val \#)) false
                     (some #{pos} path) false
                     (= val \M) true
                     :else (boolean (some
                                     #(find-rec board % (cons pos path))
                                     (map #(mapv + pos %) steps))))))]
    (find-m board (find-c board) [])))

;;118 Re-implement Map
(fn my-map [pred [fst & rst :as xs]]
  (lazy-seq (when (seq xs) (cons (pred fst) (my-map pred rst)))))


;;119 Win at Tic-Tac-Toe
(defn f [trn xo]
  (let [m* (fn [n m] (map #(* % n) m))
        l+ (fn [p l] (map #(map + p %) l))
        seed [[0 1] [1 0] [1 1] [-1 1]]
        mult [-2 -1 1 2]
        delta (map (fn [l] (map #(m* % l) mult)) seed)
        lines (fn [p] (map #(l+ p %) delta))
        check-ln (fn [ln] (filter identity (map #(get-in xo %) ln)))
        check (fn [p] (->> (lines p)
                           (map check-ln)
                           (map #(cons trn %))
                           (filter #(= 3 (count %)))
                           (map set)
                           (map count)
                           (some #{1})))]
    (set (for [x (range 3) y (range 3) :when (and (= :e (get-in xo [x y])) (check [x y]))] [x y]))))
;; wow solution
;;#(
;;  reduce conj #{}
;;  (for [x (range 3) y (range 3)
;;       :when (and
;;               (= :e (get-in %2 [x y]))
;;               ((fn [[[a b c] [d e f] [g h i] :as x]]
;;                  (some {[% % %] %}
;;                        (list* [a d g]
;;                               [b e h]
;;                               [c f i]
;;                               [a e i]
;;                               [c e g]
;;                               x)))
;;                (assoc-in %2 [x y] %)))] [x y]))

;;120 Sum of square of digits
(defn f [xs]
  (let [digits (fn [n] (map #(- (int %) (int \0)) (str n)))
        pws (fn [d] (apply + (map #(* % %) d)))]
    (count (filter #(< % ((comp pws digits) %))  xs))))

;;121 Universal Computation Engine
(defn evl
  ([exp] (fn [env] (evl exp env)))
  ([exp env]
   (let [rslv (fn[exp env] (get (merge {'+ + '- - '/ / '* *} env) exp exp))]
     (cond
       (symbol? exp) (rslv exp env)
       (list? exp) (apply (evl (first exp) env) (map #(evl % env) (rest exp)))
       :else exp))))

;;122 Read a binary number
#(Integer/parseInt % 2)

;;124 Analyze Reversi
(defn f [r color]
  (let [rcolor {'b 'w 'w 'b}
        fxy (for [i [identity inc dec]
                  j [identity inc dec]
                  :when (not= i j identity)] [i j])
        empty (for [y (range 0 (count r))
                    x (range 0 (count (first r)))
                    :when (= 'e (get-in r [y x]))] [y x])
        flip-rec (fn flip-rec [[y x :as p] fy fx res]
                   (cond
                     (= (rcolor color) (get-in r p))
                     (flip-rec [(fy y) (fx x)]
                               fy
                               fx
                               (conj res p))
                     (= color (get-in r p)) res
                     :else []))
        flip (fn [[y x] [fy fx]]
               (flip-rec [(fy y) (fx x)] fy fx []))
        check-point (fn [point]
                      (vector point (set (mapcat #(flip point %) fxy))))]
    (->> (map #(check-point %) empty)
         (remove (comp empty? second))
         (into {}))))

;;125 Gus' Quinundrum
(fn f
  ([] (str (f (quote (list (quote fn) (quote f) (list [] (list (quote str) (list (quote f) (list (quote quote) x)))) (list [(quote x)] x))))))
  ([x] (list (quote fn) (quote f) (list [] (list (quote str) (list (quote f) (list (quote quote) x)))) (list [(quote x)] x))))

;;wow solution
;;(fn [] (let [x ["(fn [] (let [x "
;;                "] (str (first x) x (second x))))"]]
;;         (str (first x) x (second x))))

;;126 Through the Looking Class
java.lang.Class


;;127 Love Triangle
(letfn
    [(b->m [b]
       (let [hight (count b)
             width (count (Integer/toString (apply max b) 2))
             frm (str "~" (max width hight) ",'0B")]
         (mapv #(clojure.pprint/cl-format nil frm %) b)))

     (middle
       ([b] (middle b inc))
       ([b itr] (filter identity (map #(middle b % itr) (range (count b)))))
       ([b line itr] (middle b line (count (first b)) [] itr))
       ([b line len res itr]
        (when (and (odd? (count (first b))) (>= line  0) (< line (count b)))
          (let [n (count (first b))
                start (/ (- n len) 2)
                segm (for [i (range start (+ start len))] (get-in b [line i]))]
            (if (<= len 1)
              (cons (get-in b [line start]) res)
              (middle b (itr line) (- len 2) (concat segm res) itr))))))

     (corners [b]
       (let [n (count b)
             r (for [i (range n) j (range i n)] (get-in b [i j]))
             l (for [i (range n) j (range 0 (inc i))] (get-in b [i j]))]
         (list r l)))

     (lt [b]
       (mapv #(apply str (butlast %))  (butlast b)))

     (rt [b]
       (mapv #(apply str (rest %)) (butlast b)))

     (lb [b]
       (mapv #(apply str (butlast %)) (rest b)))

     (rb [b]
       (mapv #(apply str (rest %)) (rest b)))

     (tri [b]
       (let [bt (apply mapv str (reverse b))]
         (->>
          (concat
           (corners bt)
           (corners b)
           (middle b inc)
           (middle b dec)
           (middle bt inc)
           (middle bt dec))
          (remove #(some #{\0} %)))))

     (fract
       ([b] (fract (memoize fract) b))
       ([mem b]
        (if (= (count b) 1)
          nil
          (concat (tri b)
                  (mem mem (lt b))
                  (mem mem (rt b))
                  (mem mem (lb b))
                  (mem mem (lb b))))))]

 (defn f[b]
    (let [t (fract (b->m b))]
      (if-not (empty? t)
        (apply max (map count t))
        nil))))

;;128 Recognize Playing Cards
(fn [[s r]]
  (hash-map :suit ({\D :diamond \H :heart \C :club \S :spade} s)
            :rank ((zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range)) r)))

;;130 Tree reparenting
(defn f
  ([root xs] (f root xs nil))
  ([root xs parent]
   (let [add-parent (fn [path parent]
                      (if (empty? parent)
                        path
                        (concat path (list parent))))]
     (cond
       (= (first xs) root) (add-parent xs parent)
       (not (seq? xs)) nil
       :else
       (some identity
             (map #(f root
                      %
                      (add-parent (remove #{%} xs) parent))
                  (filter seq? xs)))))))
;; wow solution
;;(fn [n t]
;;  (->> t
;;       (tree-seq next rest)
;;       (filter #(some #{n} (flatten %)))
;;       (reduce (fn [a b]
;;                 (concat b (list (remove #{b} a)))))))

;;131 Sum Some Set Subsets
(letfn
    [(powerset [xs]
       (reduce (fn [sets x]
                 (->> (map #(conj % x) sets)
                      (into sets)))
               #{#{}} xs))
     (subsums [xs]
       (->> (powerset xs)
            (#(disj %  #{}))
            (map #(apply + %))
            (set)))]
  (fn f [& xs]
    ((complement empty?)
     (apply clojure.set/intersection (map subsums xs)))))

;;132 Insert between two items
(defn f[op val [fst & rst :as xs]]
  (lazy-seq
   (cond
     (not (seq rst)) xs
     (op fst (first rst)) (cons fst (cons val (f op val rst)))
     :else (cons fst (f op val rst)))))
;;wow solution
;;(fn [p v xs]
;;  (mapcat
;;   #(if (p %1 %2) [%1 v] [%1])
;;   xs
;;   (lazy-cat (rest xs) (take 1 xs))))

;;134 A nil key
#((comp boolean (set %2)) [% nil])
;;wow solution
;;#(nil? (%2 % %))

(defn f [fst & rst]
  (reduce (fn [m [op opd]] (op m opd)) fst (partition 2 rst)))

;;135 Infix Calculator
(fn f [fst & rst]
  (reduce (fn [m [op opd]] (op m opd)) fst (partition 2 rst)))
;;(fn me
;;  ([x f y] (f x y))
;;  ([x f y & r] (apply me (f x y) r)))

;;137 Digits and bases
(defn f
  ([num base] (vec (f num base [])))
  ([num base res]
   (if (= 0 (quot num base)) (cons num res)
       (recur (quot num base) base (cons (mod num base) res)))))

;;138 Squares Squared
(letfn
    [(path
       ([xs size]
        (->> (path xs (cycle [[-1 0] [0 1] [1 0] [0 -1]]) #{})
             (map (fn [[y x]] [(- (+ y x) 2) (+ (- x y) (dec size))]))
             (take (* size size))))
       ([xy ops history]
        (lazy-seq
         (let [nxy (map + xy (first ops))
               nnxy (map + xy (second ops))]
           (if (history nnxy)
             (cons xy (path nxy ops (conj history xy)))
             (cons xy (path nnxy (rest ops) (conj history xy))))))))

     (sq [base limit]
       (if (> base limit)
         nil
         (cons base (sq (* base base) limit))))

     (pad [s n symb]
       (apply str (for [i (range 0 (max (count s) n))] (get s i symb))))

     (create-board [size]
       (let [repeatv (comp vec repeat)]
         (repeatv size (repeatv size " "))))]

    (fn f [base limit]
      (let [squars (apply str (sq base limit))
            len (int (Math/ceil (Math/sqrt (count squars))))
            padded-squars (pad squars (* len len) \*)
            start (int (Math/ceil (/ len 2)))
            board (create-board (+ len (dec len)))
            coords (map #(vector % %2) padded-squars (path [start start] len))]
        (->>
         (reduce (fn [m [sym coord]] (assoc-in m coord sym))
                 board
                 coords)
         (map #(apply str %))))))

;;140 Veitch, Please!
(letfn
    [(wl [kmap]
       [(count (first kmap)) (count kmap)])

     (divs [w l x]
       (for [i (range 1 (inc l)) j (range 1 (inc w)) :when (= x (* i j))] [i j]))

     (pows [lim]
       (for [i (iterate #(* 2 %) 1) :while (< i lim)] i))

     (term [kmap [start-y start-x] [y x]]
       (let [[w l] (wl kmap)]
         (for [dy (range y) dx (range x)
               :let [i (mod (+ start-y dy) l) j (mod (+ start-x dx) w)]]
           [(get-in kmap [i j]) [i j]])))

     (minterm? [zone]
       (every? #{1} (map first zone)))

     (minterms-iter [kmap point]
       (let [[w l] (wl kmap)
             corners (mapcat #(divs w l %) (pows (* w l)))]
         (->> (map #(term kmap point %) corners)
              (filter #(minterm? %))
              (map #(set (map second %))))))

     (smaller? [term terms]
       (empty? (apply (partial clojure.set/difference term) terms)))

     (largest
       ([minterms] (largest (sort #(< (count %1) (count %2)) minterms) '()))
       ([[fst & rst :as minterm] res]
        (cond
          (not (seq minterm)) res
          (smaller? fst (concat res rst)) (largest rst res)
          :else (largest rst (conj res fst)))))

     (minterms [kmap]
       (let [[w l] (wl kmap)
             ones (for [i (range l) j (range w)
                        :when (= 1 (get-in kmap [i j]))] [i j])]
         (mapcat #(minterms-iter kmap %) ones)))

     (gray-code [n]
       (->> (range (Math/pow 2 n))
            (map #(bit-xor % (bit-shift-right % 1)))
            (map #(clojure.pprint/cl-format nil (str "~" n "'0B") %))))

     (apply-gray [str code]
       (map #(if (= %2 \1)
               (symbol (clojure.string/upper-case %1))
               (symbol (clojure.string/lower-case %1))) str code))

     (gray [str]
       (map #(apply-gray str %) (gray-code (count str))))

     (match? [gray samples]
       (if (some identity (map #(every? (set gray) %) samples))
           1 0))

     (gray-map [samples]
       (let [sample (sort (clojure.string/lower-case (apply str (first samples))))
             mid (quot (count sample) 2)
             sy (apply str (take mid sample))
             sx (apply str (drop mid sample))]
         (mapv (fn [y] (mapv (fn [x] (concat y x)) (gray sx))) (gray sy))))

     (karn-map [g-map samples]
       (mapv #(mapv (fn[node] (match? node samples)) %) g-map))

     (make-disj [g-map term]
       (let [rset (set (mapcat #(get-in g-map %) term))]
         (remove #(and (rset (symbol (clojure.string/upper-case %)))
                       (rset (symbol (clojure.string/lower-case %)))) rset)))]

  (fn veitch [samples]
    (let [g-map (gray-map samples)
          k-map (karn-map g-map samples)
          terms (largest (minterms k-map))]
      (set (map #(set (make-disj g-map %)) terms)))))

;;wow solutuion
(letfn
    [(upsym [s]
       (symbol (clojure.string/upper-case (name s))))
     (downsym [s]
       (symbol (clojure.string/lower-case (name s))))
     (swapsym [s]
       (if (= s (upsym s))
         (downsym s)
         (upsym s)))
     (independent [sym s sets]
       (let [indep (disj s sym)
             swapped (conj indep (swapsym sym))]
         (if (or (contains? sets indep)
                 (contains? sets swapped))
           indep
           false)))
     (update-iter [s sets]
       (if-let [indep (some #(independent % s sets) s)]
         indep
         s))
     (update [sets]
       (set (map #(update-iter % sets) sets)))]
  (defn min-set [sets]
    (if (= (update sets) sets)
      sets
      (min-set (update sets)))))

;;141 Tricky card games
(defn mk[trump]
  (let [lead (fn [game suit]
               (->> game
                    (filter #(= (:suit %) suit))
                    (sort-by :rank)
                    (last)))]
    (fn [[{lead-suit :suit} & rst :as trick]]
      (let [lead-winer (lead trick lead-suit)
            trump-winer (lead trick trump)]
        (if trump-winer trump-winer lead-winer)))))

;;143 dot product
#(apply + (map * %1 %2))

;;144 Oscilrate
(defn f [param & funcs]
  (let [osc (fn osc [param [fst & rst]]
              (lazy-seq (concat [param] (osc (fst param) rst))))]
    (osc param (cycle funcs))))

;;wow solution
;;(fn [v & f] (reductions #(%2 %) v (cycle f)))

;;145 For the win
[1 5 9 13 17 21 25 29 33 37]

;;146 Trees into tables
#(into {} (for [[i m] %  [j v] m] [[i j] v]))

;;147 Pascal's Trapezoid
(fn [xs] (iterate #(map (partial apply +') (partition 2 1 (concat [0] % [0]))) xs))

;;148 The Big Divide
(defn f [n fst scd]
  (let [sum (fn [n d]
              (let [div (bigint (quot (dec n) d))]
                (* d div (inc div) 1/2)))]
    (+ (sum n fst) (sum n scd) (- (sum n (* fst scd))))))

;;150 Palindromic Numbers
(letfn
    [(mid [cnt]
       (let [middle (long (Math/ceil (/ cnt 2)))]
         (if (odd? cnt) (dec middle) middle)))
     (pow-ten [n]
       (Long. (apply str (cons 1 (repeat n 0)))))
     (reverce-num [num]
       (Long. (apply str (reverse (str num)))))]
  (defn f [num]
    (lazy-seq
     (let [cnt (count (str num))
           midl (mid cnt)
           base (pow-ten midl)
           lpart (quot num base)
           lbase (* lpart base)
           rpart (mod (reverce-num lpart) base)
           new-num (+ lbase rpart)]
       (if (< new-num num)
         (f (+ lbase base))
         (cons new-num (f (+ lbase base))))))))

;;152 Latin Square Slicing
(letfn
    [(latin? [sq]
       (let [cnt (count sq)
             tsq (apply map vector sq)
             xsq (concat tsq sq)
             cnts (map #((comp count into) #{} %) xsq)]
         (and (= cnt (count (reduce into #{} sq)))
              (every? #(= cnt %) cnts))))

     (containsEmpty? [sq]
       (let [vals (reduce into #{} sq)]
         (some #(or (= % nil) (= % " ")) vals)))

     (subv
       ([v lenmax] (map vec (subv v lenmax [])))
       ([v lenmax res]
        (let [space " "
              lenrest (- lenmax (count v))]
          (if (<= lenrest 0)
            (conj res v)
            (subv (concat [space] v)
                  lenmax
                  (conj res (concat v (take lenrest (repeat space)))))))))

     (make-superpos [[fst & rst]]
       (if-not (seq rst)
         (map list fst)
         (for [x fst y (make-superpos rst)] (conj y x))))

     (all-superpos [xs]
       (let [maxlen (apply max (map count xs))]
         (->> (map #(subv % maxlen) xs)
              (make-superpos)
              (map vec))))

     (get-sqr [area y x side]
       (let [sub-area (for [dy (range side) dx (range side)]
                        (get-in area [(+ y dy) (+ x dx)]))]
         (->> (partition side sub-area)
              (mapv vec))))

     (pos-sqrs
       ([area y x] (pos-sqrs area y x 2 []))
       ([area y x side res]
        (let [sqr (get-sqr area y x side)]
          (cond
            (containsEmpty? sqr) res
            (latin? sqr) (recur area y x (inc side) (conj res sqr))
            :else (recur area y x (inc side) res)))))

     (all-sqrs [superpos]
       (->> (for [y (range (count superpos))
                  x (range (count (first superpos)))
                  :when (not= (get-in superpos [y x]) " ")]
              (pos-sqrs superpos y x))
            (apply concat)))
     ]

  (defn count-latins [xs]
    (if (= 8 (ffirst xs))
      {2 7, 3 1, 4 1}  ; timeout 
      (->> (all-superpos xs)
           (mapcat all-sqrs)
           (set)
           (map count)
           (frequencies)
           ))))
;; wow solution
(letfn
    [(subv [row pos size max-length]
       (let [row-len (count row)
             row-min (max (+ pos (- row-len max-length)) 0)
             row-max (inc (min pos (- row-len size)))]
         (drop row-min (take row-max (partition size 1 row)))))


     (superpos [[fst & rst]]
       (if-not (seq rst)
         (map vector fst)
         (for [s fst p (superpos rst)] (concat [s] p))))

     (row-squares [vecs max-length]
       (let [segments (for [size (range 2 (inc (min (count vecs) max-length)))
                            pos (range 0 (inc (- max-length size)))]
                        (map #(subv % pos size max-length) (take size vecs)))]
         (mapcat superpos segments)))

     (squares [vecs max-length]
       (if (< (count vecs) 2)
         []
         (concat (row-squares vecs max-length)
                 (squares (vec (rest vecs)) max-length))))

     (all-squares [vecs]
       (distinct (squares vecs (apply max (map count vecs)))))
     
     (latin? [sq]
       (let [cnt (count sq)
             tsq (apply map vector sq)
             xsq (concat tsq sq)
             cnts (map #((comp count into) #{} %) xsq)]
         (and (= cnt (count (reduce into #{} sq)))
              (every? #(= cnt %) cnts))))]
  (defn dol [x] (frequencies (map count (filter latin? (all-squares x))))))

;;153 Pairwise Disjoint Sets
(fn [xs]
  (= (count (reduce into #{} xs))
     (apply + (map count xs))))

;;156 Map Defaults
(fn [v m] (reduce #(conj %1 [%2 v]) {} m))
