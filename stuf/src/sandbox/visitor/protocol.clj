(ns sandbox.visitor.protocol)
;; expression problem


;; initial domain
(defprotocol NameExtractor
  (extract-name [this] "Extracts a name from a person."))

(defrecord SimplePerson [first-name last-name house-num street])

(def simple-person (->SimplePerson "FirstName" "SecodName" 123 "Street"))

(extend-type SimplePerson
  NameExtractor
  (extract-name [this]
    (str (:first-name this) " " (:last-name this))))

;; add a new type
(defrecord ComplexPerson [name address]
  NameExtractor
  (extract-name [this]
    (str (-> this :name :first) " " (-> this :name :last))))

(def complex-person (->ComplexPerson {:first "First" :last "Last"}
                                     {:house-num 123 :street "Fake St."}))

;; add a new operation
(defprotocol AddressExtractor
  (extract-address [this] "Extracts address from a person."))

(extend-type SimplePerson
  AddressExtractor
  (extract-address [this]
    (str (:house-num this) " " (:street this))))

(extend-type ComplexPerson
  AddressExtractor
  (extract-address [this]
    (str (-> this :address :house-num)
         " "
         (-> this :address :street))))
