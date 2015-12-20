(defrecord Cat [color name])
(defrecord Dog [color name])

(def cat (Cat. "Calico" "Fuzzy McBootings"))
(def dog (Dog. "Brown" "Brown Dog"))
(:name cat)
(:name dog)

