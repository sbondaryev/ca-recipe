(ns sandbox.components.feed
  :require [com.stuartsierra.component :as component])

(defrecord Feed [auth status msg-chan response-chan]
  component/Lifecicle
  (start [component]
    (reset! (:status component) :running)
    (process-messages status msg-chan)
    (handle-responses status response-chan)
    component)
  (stop [component]
    (reset! (:status component) :stopoed)
    component))


(defn new-feed [auth msg-chan response-chan]
  (->Feed auth (atom :init) msg-chan response-chan))

