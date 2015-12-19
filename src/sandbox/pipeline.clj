(ns ca-recipe.pipeline)

(def parse-words (map #(set (clojure.string/split % #"\s"))))

(def interesting (filder #(contains? % "Clojure")))

(defn match [search-words message-words]
  (count (clojure.set/intersection search-words message-words)))

(defn happy (partial match #{"happy" "awsome" "rocks" "amazing"}))
(defn sad (partial match #{"sad" "bug" "crach"}))
(defn score (map #(hash-map :words %1
                            :happy (happy %1)
                            :sad (sad %1))))

(defn sentiment-stage
  [in out]
  (let [xf (comp parse-words interesting score)]
    (async/pipeline 4 out xf in)))

(defn interesting-stage
  [in intermediate]
  (let [xf (comp parse-words interesting)]
    (async/pipeline 4 intermediate xf in)))

(defn score-stage
  [intermediate out]
  (async/pipeline 1 out score intermediate))

(defn assemble-stages
  [in out]
  (let [intermediate (async/chan 100)]
    (interesting-stage in intermediate)
    (score-stage intermediate out)))
