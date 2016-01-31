(ns sandbox.multithread
  (:require [instaparse.core :as insta])
  (:require [clojure.zip :as z])
  (:require [clojure.pprint :refer :all]))

(def p pprint)

(defn p->i [{:keys [instructions] :as process}]
  (map #(merge (dissoc process :instructions) %) instructions))

(defn ps->is [processes]
  (mapcat p->i processes))

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

(def program
  "heavy-op op1;
  light-op op2;
  lock l1;
  medium-op op3;
 unlock l1;")

(defn fire-process [grammar program process-id priority]
  {:process-id process-id
   :instructions (flatten (gen-program (insta/parser grammar) program))
   :priority priority})

(def insts-effort {:heavy-op 10 :medium-op 5 :light-op 2 :lock 1 :unlock 1})

(defn all-lock-indices [instructions]
  (->> instructions
       (filter #(= (:inst-type %) :lock))
       (map #(hash-map :lock-id (% :inst-id) :lock-idx (.indexOf instructions %)))))

(defn the-locks-inst-depends-on
  [instructions instruction]
  (let [the-inst-idx (.indexOf instructions instruction)]
    (->> (all-lock-indices instructions)
         (filter #(> the-inst-idx (:lock-idx %) ))
         (map :lock-id)
         (into []))))

(defn lock [locks process-id lock-id]
  (assoc locks lock-id {:locker process-id :locked true}))

(defn unlock [locks process-id lock-id]
  (assoc locks lock-id {:locker process-id :locked false}))

(defn is-locked? [process-id instructions locks instruction]
  (->> (the-locks-inst-depends-on instructions instruction)
       (some #(and (not= process-id ((get locks %) :locker))
                   ((get locks %) :locked)))))

(defn incomplete-instruction? [instruction-w-count]
  (let [instr-effort (insts-effort (instruction-w-count :inst-type))
        instr-count (count (instruction-w-count :times))]
    (< instr-count instr-effort)))

(defn incomplete-process? [process-w-counts]
  (let [instrs-w-count (process-w-counts :instructions)]
    (some incomplete-instruction? instrs-w-count)))

(defn more-incomplete-processes? [processes-w-count]
  (some incomplete-process? processes-w-count))

(defn find-inst-to-fire [locks process-id instructions scheduled-parts]
  (let [not-locked-instrs (set (->> instructions
                                      (filter #(not (is-locked? process-id
                                                                instructions
                                                                locks
                                                                %)))))
        incomplete-instrs (set (->> (p->i  scheduled-parts)
                                      (filter incomplete-instruction?)
                                      (map #(dissoc % :times))))
        fireable-instrs (clojure.set/intersection not-locked-instrs
                                                  incomplete-instrs)
        instr-id-to-fire (->> fireable-instrs
                              (sort-by #(.indexOf instructions %) < )
                              (first)
                              (:inst-id))]
    instr-id-to-fire))


(defn progress-on-process! [locks-ref scheduled-ref process quantum]
  (let [process-instrs (p->i process)
        processes-scheduled-parts @scheduled-ref
        process-scheduled-parts (->> processes-scheduled-parts
                                     (filter #(= (:process-id %)
                                                 (:process-id process)))
                                     (first))]
    (if-let [instr-to-fire-id (find-inst-to-fire @locks-ref
                                                 (:process-id process)
                                                 process-instrs
                                                 process-scheduled-parts )]
      (dosync
       (let [instr-to-fire (->> process-instrs
                                (filter #(= (:inst-id %)
                                            instr-to-fire-id))
                                (first))]
         (cond
           (= (:inst-type instr-to-fire) :lock ) (alter locks-ref lock (:process-id process) instr-to-fire-id)
           (= (:inst-type instr-to-fire) :unlock ) (alter locks-ref unlock (:process-id process) {:lock (:unlock instr-to-fire-id)})))

       (let [p-in-scheduled (->> @scheduled-ref
                                 (filter #(= (:process-id %)
                                             (:process-id process)))
                                 (first))
             instr-in-p-in-scheduled (->> (get p-in-scheduled :instructions)
                                          (filter #(= (:inst-id %)
                                                      instr-to-fire-id))
                                          (first))
             idx-p-in-scheduled (max 0 (.indexOf @scheduled-ref p-in-scheduled))
             idx-inst-in-p-in-scheduled (max 0 (.indexOf  (get p-in-scheduled :instructions) instr-in-p-in-scheduled))
             times-in-inst-in-p-in-scheduled (get
                                              (get (p-in-scheduled
                                                    :instructions)
                                                   idx-inst-in-p-in-scheduled) :times )
             _ (alter scheduled-ref assoc-in [idx-p-in-scheduled
                                              :instructions idx-inst-in-p-in-scheduled :times]
                      (conj times-in-inst-in-p-in-scheduled
                            quantum))])
       true)
      false)))


(defn add-times [instructions]
  (vec (map #(assoc % :times []) instructions)))

(defn prepare-scheduled [processes]
  (vec (map #(update % :instructions add-times) processes)))

(defn prepare-locks [processes]
  (->> (ps->is processes)
       (filter #(= (:inst-type %) :lock))
       (map #((juxt :process-id :inst-id) %))
       (reduce (partial apply unlock) {} )))

(defn gen-processes-cycles [processes]
  (->> (sort-by :priority > processes)
       (mapcat #(repeat (:priority %) %))
       (cycle)))

(defn total-time [processes]
  (->> (ps->is processes)
       (map :inst-type)
       (map insts-effort)
       (reduce +)
       (* 2)))

(defn parse-processes [language programs]
  (vec (map #(fire-process language
                             (:program %)
                             (:process-id %)
                             (:priority %))
            programs)))

(defn schedule-programs [language programs]
  (let [processes (parse-processes language programs)
        timeout (total-time processes)
        locks (ref (prepare-locks processes))
        scheduled (ref (prepare-scheduled processes))
        processes-cycles  (gen-processes-cycles processes)]
    (loop [quantum 0
           remaining-processes processes-cycles]
      (if (and (more-incomplete-processes? (scheduled-processes-parts @scheduled))
               (< quantum timeout))
        (do
          (progress-on-process! locks scheduled
                                (first remaining-processes)
                                quantum)
          (recur (inc quantum)
                 (next remaining-processes)))
        @scheduled))))

(def programs
  [{:priority 3,
    :program
    "heavy-op op1;light-op op2;lock l1;medium-op op3;unlock l1;",
    :process-id :pr1}
   {:priority 1,
    :program "lock l1;medium-op op4;unlock l1;medium-op op5;",
    :process-id :pr2}])
