(ns sandbox.strategy.strategy)

(defn first-name-valid? [person]
  (not (nil? (:first-name person))))

(defn full-name-valid? [person]
  (and
   (not (nil? (:first-name person)))
   (not (nil? (:middle-name person)))
   (not (nil? (:last-name person)))))

(defn person-collector [valid?]
  (let [valid-people (atom [])]
    (fn [person]
      (if (valid? person)
        (swap! valid-people conj person))
      @valid-people)))
