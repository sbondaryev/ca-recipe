(defrecord Cat [color name])
(defrecord Dog [color name])

(def cat (Cat. "Calico" "Fuzzy McBootings"))
(def dog (Dog. "Brown" "Brown Dog"))
(:name cat)
(:name dog)

;type polimorphism with Record/Protocol
(defprotocol NoiseMaker
  (make-noise [this]))

(defrecord NoisyCat [color name]
  NoiseMaker
  (make-noise [this] (str (:name this) "meows!")))

(defrecord NoisyDog [color name]
  NoiseMaker
  (make-noise [this] (str (:name this) "barks!")))

(def noisy-cat (NoisyCat. "Calico" "Fuzzy McBootings"))
(def noisy-dog (NoisyDog. "Brown" "Brown Dog"))
