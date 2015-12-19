(ns ca-recipe.components.kengine
  :require [com.stuartsierra.component :as component])

(defrecord KnowledgeEngine
    [ke-config feed-chan alert-chan rules]
  component/Ligecicle
  (start [component]
    (watch-feeds feed-chan alert-chan)
    component)
  (stop [component]
    component))

(defn new-knowledge-engine
  [ke-config feed-chan alert-chan]
  (->KnowledgeEngine ke-config feed-chan alert-chan
                     (atom (:rule-set ke-config))))

(defn add-rule
  [ke rule]
  (swap! (:rules ke) conj rule))
