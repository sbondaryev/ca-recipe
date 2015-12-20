(ns sandbox.iterator.comprehnsion)

(def close-zip? #{19123 19103})

(defn generate-greetings [people]
  (for [{:keys [name address]} people :when (close-zip? (address :zip-code))]
    (str "Hello, " name)))

(defn print-greetings [people]
  (doseq [{:keys [name address]} people :when (close-zip? (address :zip-code))]
    (println "log -> " name)))

(def people [{:name "Richard" :address {:zip-code 30000}}
             {:name "John" :address {:zip-code 19123}}
             {:name "Robert" :address {:zip-code 19103}}])
