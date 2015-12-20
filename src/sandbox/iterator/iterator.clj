(ns sandbox.iterator.iterator)

(def vowel? #{\a \e \i \o \u})
(defn vowels-in-world [world]
  (set (filter vowel? world)))


(defn prepend-hello [names]
  (map (fn [name] (str "Hello, " name)) names))


(defn sum-sequence [s]
  {:pre [(not (empty? s))]}
  (reduce + s))
