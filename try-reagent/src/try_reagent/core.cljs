(ns try-reagent.core
  (:require-macros [cljs.core.async.macros :refer [go-loop go]]
                   [reagent.ratom :refer [reaction]]
                   [try-reagent.macros :refer [<?]])
  (:require [reagent.core :as r]
            [cljs.core.async :as a :refer [<! put! chan pub sub timeout]]))

;; --- state
(defonce state (r/atom {:a 1 :b 2}))
(defn get-key [k]
  (r/cursor state [k]))
(defn update-a [v]
  (swap! state #(assoc-in % [:a] v)))

(defn do-async [k]
  (println k))

;; --- Action
(def events (chan))
(def submits (chan))
(def logs (chan))
(def pub-events (pub events :event))
(sub pub-events :log logs)
(sub pub-events :submit submits)

(defn async-call [data time]
  (let [out (chan)]
    (go
      (<! (timeout time))
      (put! out (js/Error. "Opps!")))
    out))

(defn mount-events []
  (go-loop []
    (let [{data :payload} (<! submits)]
      (try
        (update-a (<! (async-call data 1000)))
        (update-a (<? (async-call data 2000)))
        (catch js/Error e (update-a (str "Catched")))))
    
    (recur))
  (go-loop []
    (println (<! logs))
    (recur)))


;; --- View

(defn input-a []
  (println "render input-a")
  [:div
   "a: "
   [:input {:type "text"
            :value (:a @state)
            :on-change #(update-a (-> % .-target .-value))}]])

(defn submit-a []
  (print "render submit-a")
  [:button {:on-click #(put! events {:event :submit :payload @(get-key :a)})} "Submit"])

(defn log-a []
  (print "render log-a")
  [:button {:on-click #(put! events {:event :log :payload @(get-key :a)})} "Log"])


(defn view-a []
  (let [out @(get-key :a)]
    (println "render a")
    [:div (str "a: " out)]))

(defn home-page []
  [:div
   [input-a]
   [view-a]
   [submit-a]
   [log-a]
   ])


(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-events)
  (mount-root))
