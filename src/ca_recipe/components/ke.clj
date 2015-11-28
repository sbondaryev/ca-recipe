(ns ca-recepie.components.ke)

(defprotocol KE
  (get-rules [ke] "Get full rule set")
  (transform-rules [ke update-fn]
                   "Apply transformation function to rule set. Return new KE")
  (fire-rules [ke request]
              "Fire the rules against the request and return a response"))

;; private functions
(defn- transform-criteria [criteria])

;; api build over the protocol
(defn find-rules
  [ke criteria]
  (filter (transform-criteria criteria) (get-rules ke)))

(defn add-rule
  [ke rule]
  (transform-rules ke #(conj % rule)))

(defn replace-rule
  [ke old-rule new-rule]
  (transform-rules ke #(-> % (disj old-rule) (conj new-rule))))

(defn delete-rule
  [ke rule]
  (tranform-rules ke #(-> % (disj rule))))

(defrecord KnowledgeEngine
    [config
     ch-in
     ch-out
     rules
     active])
  
(defn make-knowledge-engine
  [config ch-in ch-out rule-set]
  (->KnowledgeEngine config ch-in ch-out (atom rule-set) (atom false)))

(defn start-knowledge-engine
  [{:keys (ch-in ch-out rules active) :as ke}] (reset! active true)
  (go-loop [request (<! ch-in)
            response (fire-rules ke request)]
    (>! ch-out response)
    (when @active (recur))) ke)

(defn stop-knowledge-engine
  [{:keys (ch-out active) :as ke}]
  (reset! active false) ;; exit go loop
  (async/close! ch-out) ;; stop producing
  ke)
