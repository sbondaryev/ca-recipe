(ns recipe3.core
  (:require [instaparse.core :as insta])
  (:require [clojure.zip :as z])
  (:require [clojure.pprint :refer :all]))

(def p pprint)

(def r3-language
"S = INSTRS
  INSTRS = ((INSTR | LOCKED-INSTRS) <optional-whitespace>)*
  INSTR = HEAVY-OP | MEDIUM-OP | LIGHT-OP
  HEAVY-OP = <optional-whitespace> 'heavy-op' <whitespace> ID <SEP>
  MEDIUM-OP = <optional-whitespace> 'medium-op' <whitespace> ID <SEP>
  LIGHT-OP = <optional-whitespace> 'light-op' <whitespace> ID <SEP>
  
  LOCKED-INSTRS = LOCK INSTRS UNLOCK
  LOCK = <optional-whitespace> 'lock' <whitespace> ID <SEP>
  UNLOCK = <optional-whitespace> 'unlock' <whitespace> ID <SEP>
  
  ID = #'[a-zA-Z0-9]+'
  PRIORITY = #'[0-9]+'

  whitespace = #'\\s+'
  optional-whitespace = #'\\s*'
  SEP = #'\\s*' ';'")

(defn gen-program [parser program]
  (insta/transform
   {:S identity
    :INSTRS (fn [& args] (vec args))
    :INSTR identity
    :HEAVY-OP (fn [x y] {:inst-type :heavy-op :inst-id (get y 1)})
    :MEDIUM-OP (fn [x y] {:inst-type :medium-op :inst-id (get y 1)})
    :LIGHT-OP (fn [x y] {:inst-type :light-op :inst-id (get y 1)})
    :LOCKED-INSTRS (fn [& args] (vec args))
    :LOCK (fn [x y] {:inst-type :lock :inst-id {:lock (get y 1)}})
    :UNLOCK (fn [x y] {:inst-type :unlock :inst-id {:unlock  (get y 1)}})}
   (parser program)))

(defn fire-a-process [grammar program process-id priority]
  (let [prsr (insta/parser grammar)
        vec-instructions (gen-program prsr program)
;;=> the nested structure
        zpr (z/vector-zip vec-instructions)]
    (loop [result []
           loc (->  zpr z/down)]
      (if (z/end? loc)
;;=> the end of recursion, no more nodes to visit
        {:process-id process-id
         :instructions result
         :priority priority}
;;=> We generate the process
        (recur (if (map? (z/node loc))
;;=> We only append to result the elements of type 'map'
                 (conj result (z/node loc))
                 result)
;;=> else we pass result as is in the recursion
               (z/next loc))))))
;;=> and we recur with the next element.

(def insts-effort {:heavy-op 10 :medium-op 5 :light-op 2 :lock 1 :unlock 1})

(defn all-locks-indices  [instructions]
  ;;=> 'instructions' is the ':instructions vector' of the output of
  ;; fire-process.
  (let [locks (filter #(= (:inst-type %) :lock) instructions)
        ;;=> We find out all the 'locks' in 'instructions'.
        lock-indices (map (fn [l] {:lock-id (l :inst-id)
                                   :lock-idx (.indexOf
                                              instructions l)})
                          locks)]
    ;; And for every lock we find out its index in 'instructions,
    ;; and prepare a map with it.
    lock-indices))
;;=> output of this is : ({:lock-id {:lock "l1"}, :lock-idx 2})

(defn the-locks-inst-depends-on
  [instructions instruction]
  (let [the-inst-idx (.indexOf instructions instruction)
        the-lock-idxs (all-locks-indices instructions)]
    (into []  (->> the-lock-idxs
                   (filter #(> the-inst-idx (:lock-idx %) ))
                   (map :lock-id)))))

(defn lock
  "locks lock lock-id in locks map"
  [locks process-id lock-id]
  (assoc locks lock-id {:locker process-id :locked true}))
(defn unlock
  "unlocks lock lock-id in locks map"
  [locks process-id lock-id]
  (assoc locks lock-id {:locker process-id :locked false}))
;;=> The locks state contains its locked state and which process
;; did lock it.

(defn is-locked?
  [process-id
   instructions
   locks
   instruction]
  (let [inst-locks (the-locks-inst-depends-on instructions
                                              instruction)]
    (some true? (map #(and (not= process-id ((get locks %)
                                             :locker))
                           ((get locks %) :locked))
                     inst-locks))))
;;=> If some of the locks the instruction depend on are locked (:locked true)
;; and the locker is not its process, then it is considered as
;; locked.

(defn scheduled-processes-parts
  [scheduled]
  (into [] (map  (fn [p] {:process-id (:process-id p)
                          :instructions (into []
                                              (map (fn [i] {:inst-id (:inst-id i)
                                                            :inst-type (:inst-type i)
                                                            :count (count (:times i))})
                                                   (:instructions   p)))})
                 scheduled)))
;;=> this functions just adds :count n to the map maintained in
;;"scheduled"

(defn incomplete-instruction?
  [instruction-w-count]
  (let [instr-effort (insts-effort (instruction-w-count :inst-type))
        instr-count (instruction-w-count :count)]
    (< instr-count instr-effort)))

(defn incomplete-process?
  [process-w-counts]
  (let [instrs-w-count (process-w-counts :instructions)]
    (some true? (map incomplete-instruction?
                     instrs-w-count))))

(defn more-incomplete-processes?
  [processes-w-count]
  (some true? (map incomplete-process?
                   processes-w-count)))
;=> processes-w-count is just another name for the "scheduled"
;; state map.

(defn find-inst-to-be-fired-in-process
         [locks
          process-id
          the-process-instructions
          the-process-scheduled-parts]
         (let [p-not-locked-instrs (set (->> the-process-instructions
                                             (filter #(not (is-locked? process-id
                                                                       the-process-instructions
                                                                       locks
                                                                       %)))))
;;=> A set of not locked instructions
               p-incomplete-instrs (set (->> (:instructions  the-process-
                                                             scheduled-parts)
                                             (filter incomplete-instruction?)
                                             (map #(dissoc % :count))))
;;=> A set of incomplete instructions
               fireable-instrs (clojure.set/intersection p-not-locked-instrs
                                                         p-incomplete-instrs)
;;=> Their intersection
               instr-id-to-fire (->> fireable-instrs
                                     (sort-by #(.indexOf the-process-instructions %) < )
                                     (first)
                                     (:inst-id))]
;;=> The first on of them
           instr-id-to-fire))
