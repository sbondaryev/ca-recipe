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


(defn progress-on-process!
  [locks-ref
   scheduled-ref
   the-process
   quantum]
  (let [the-process-instrs (the-process :instructions)
        processes-scheduled-parts (scheduled-processes-parts @
                                   scheduled-ref)
the-process-scheduled-parts (->> processes-scheduled-parts
(filter #(= (:process-id %)
(:process-id the-process)))
                                         (first))]
;;=> Here we prepare the processes scheduled parts and take only
;; the relevant to the particular 'process-id'.
    (if-let [the-instr-to-fire-id (find-inst-to-be-fired-in-
                                   process @locks-ref
                                   (:process-id the-process)
                                   the-process-instrs
                                   the-process-scheduled-parts )]
;;=> If there is one instruction in "process-id" to be fired;
      (dosync
;;=> We use the refs, because we need to do transactions involving
;; both "scheduled" and "locks"
       (let [the-instr-to-fire (->> the-process-instrs
                                    (filter #(= (:inst-id %)
                                                the-instr-to-fire-id))
                                    (first))]
;;=> We get the entry relevant to this instruction-id
         (cond
           (= (:inst-type the-instr-to-fire) :lock ) (alter locks-ref
                                                            lock
                                                            (:process-id the-process)
                                                            the-instr-to-fire-id)
           {:lock
            (= (:inst-type the-instr-to-fire) :unlock ) (alter
                                                         unlock
                                                         locks-ref
                                                         (:process-id the-process)
                                                         (:unlock the-instr-to-fire-id)})))
;;=> If it is a "lock" or "unlock", We update the "locks" state
;;   map
       (let [p-in-scheduled (->> @scheduled-ref
                                 (filter #(= (:process-id %)
                                             (:process-id the-process)))
                                 (first))
;;=> To update the "scheduled" ref, we begin by finding the
;; ':process-d' in the processes vector
             instr-in-p-in-scheduled (->> (get p-in-scheduled
                                               :instructions)
                                          the-instr-to-fire-id))
         (filter #(= (:inst-id %)
                     (first))
;; Then We find the instruction in this process
                 idx-p-in-scheduled (max 0 (.indexOf @scheduled-ref
                                                     p-in-scheduled))
                 idx-inst-in-p-in-scheduled (max 0
                                                 (.indexOf  (get p-in-scheduled :instructions)
                                                            instr-in-p-in-scheduled))
;;=> We compute the index of the instruction; or we set it at 0
;; if it is not found, which means it is the first time it is
;; scheduled.
                 times-in-inst-in-p-in-scheduled (get
                                                  (get (p-in-scheduled
                                                        :instructions)
                                                       idx-inst-in-p-in-scheduled) :times )
;;=> We get the times vector in "scheduled" related to this
                 ;; instruction
                 _ (alter scheduled-ref assoc-in [idx-p-in-scheduled
                                                  :instructions idx-inst-in-p-in-scheduled :times]
                          (conj times-in-inst-in-p-in-scheduled
                                quantum))])
;;=> And using assoc-in, with indices and keys as a "path
;;   vector", we Update the "scheduled" ref with times vector
;;   to which we  Append the current "quantum".
       true)
;;=> If we were able to find a fireable instruction,
;;   we issue "true".
      false)))
;; => Else we issue "false".

(defn prepare-scheduled
  [processes]
  (into []  (->> processes
                 (map (fn[p] {:process-id (:process-id p)
                              :instructions (into []
                                                  (->> (:instructions p)
                                                       (map (fn [i] (assoc i
                                                                           :times [])))))})))))
;;=> We prepare "scheduled" as being the same thing as the
;;   "processes" map
;;   with empty ":times" vectors added.
(defn prepare-locks-for-a-p
  [a-process]
  (let [locks (filter #(= (:inst-type %) :lock )
                      (:instructions a-process))]
    (reduce (partial apply unlock) {} (map (fn [l] [(:process-id
                                                     a-process)
                                                    (:inst-id l)])
                                           locks))))
;;=> A helper function that will prepare "locks" set to false for
;;   instructions related to a process"
(defn prepare-locks
  [processes]
  (reduce merge (map prepare-locks-for-a-p processes)))
;;=> Applying "prepare-locks-for-a-p", we generate locks for all
;;   processes  that would run concurrently.

(defn gen-processes-cycles
  [processes]
  (let [sorted-procs-by-prio (sort-by :priority > processes)
        procs-pattern (mapcat #(repeat (:priority %)
                                       %)
                              sorted-procs-by-prio)]
    ;;=> A pattern is a single repetition "priority" times of each
    ;;   process
    (cycle procs-pattern)))
;;=> Generates an infinite sequence like we described above.

(defn process-sequential-time
  [a-process]
  (let [instructions (a-process :instructions)
        inst-types (map :inst-type instructions)
        lengths (map #(get insts-effort %) inst-types)]
    (reduce + lengths)))
;;=> We get instruction-types, grab the efforts from the "insts-
;; effort"
;;   map and sum them all up using reduce.

(defn schedule-programs
  [language programs]
;;=> programs are maps : {:program "the textual program",
;;  :process-id the-process-id
;;  :priority the-process-priority }
  (let [processes (into [] (map #(fire-a-process language
                                                 (:program %)
                                                 (:process-id %)
                                                 (:priority %))
                                programs))
;;=> Processes are constructed
        timeout (* 2 (reduce + (map process-sequential-time
                                    processes)))
;;=> "timeout" is the total length of all processes executed one
;;   after the other.
        locks (ref (prepare-locks processes))
        scheduled (ref (prepare-scheduled processes))
        processes-cycles  (gen-processes-cycles processes)]
;;=> We prepare "locks" and "scheduled" refs, and the weighted
;;   process repetitions that the scheduler will have to cycle
;;   through
    (loop [quantum 0
           remaining-processes processes-cycles]
;;=> We loop
      (if (and (more-incomplete-processes? (scheduled-processes-parts @scheduled))
               (< quantum timeout))
        (do
          (progress-on-process! locks scheduled
                                (first remaining-processes)
                                quantum)
;;=> progress on the selected process, with current "quantum"
          (recur (inc quantum)
                 (next remaining-processes)))
;;=> Go to next iteration, incrementing quantum and cycling
;;=> through the The weighted processes cycles.
        @scheduled))))

(def programs
  [{:priority 3,
    :program
    "heavy-op op1;light-op op2;lock l1;medium-op op3;unlock l1;",
    :process-id :pr1}
   {:priority 1,
    :program "lock l1;medium-op op4;unlock l1;medium-op op5;",
    :process-id :pr2}])
