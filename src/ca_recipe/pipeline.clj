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
