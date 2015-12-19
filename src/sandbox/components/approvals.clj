(ns sandbox.components.approvals
  :require [com.stuartsierra.component :as component])

(defrecord Approvals [approval-config
                      alert-chan
                      knowledge-engine
                      response-chan]
  component/Livecicle
  (start [component]
    (process-alerts alert-chan)
    (process-responses knowledge-engine response-chan)
    component)
  (stop [component]
    component))

(defn new-approvals [approval-config alert-chan response-chan]
  (map->Approvals {:approval-config  approval-config
                   :alert-chan alert-chan
                   :response-chan response-chan}))
