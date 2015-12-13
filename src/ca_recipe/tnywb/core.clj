(ns ca-recipe.tnywb.core
  (:require [clojure.string :as str]))

(defn- render [view model]
    (view model))

(defn- execute-request [http-request handler]
  (let [controller (handler :controller) view (handler :view)]
    (try
      {:status-code 200
       :body
       (render
        view
        (controller http-request))}
      (catch Exception e (.printStackTrace e) {:status-code 500 :body ""}))))

(defn- apply-filters [filters http-request]
  (let [composed-filter (reduce comp (reverse filters))]
    (composed-filter http-request)))

(defn tinyweb [request-handlers filters]
  (fn [http-request]
    (let [filtered-request (apply-filters filters http-request)
          path (http-request :path)
          handler (request-handlers path)]
      (execute-request filtered-request handler))))

(def request {:path "/greeting" :body "Mike,Joe,John,Steve"})

(defn make-greeting [name]
  (let [greetings ["Hello" "Greetings" "Salutations" "Hola"]
        greeting-court (count greetings)]
    (str (greetings (rand-int greeting-court)) ", " name)))

(defn handle-greeting [http-request]
  {:greetings (map make-greeting (str/split (:body http-request) #","))})

(defn render-greeting [greeting]
  (str "<h2>"greeting"</h2>"))

(defn greeting-view [model]
  (let [rendered-greetings (str/join " " (map render-greeting (:greetings model)))]
    (str "<h1>Friendly Greetings</h1>" rendered-greetings)))

(defn logging-filter [http-request]
  (println (str "In Logging Filter - request for path: " (:path http-request)))
  http-request)

(def request-handlers
  {"/greeting" {:controller handle-greeting :view greeting-view}})

(def filters [logging-filter])
(def tinyweb-instance (tinyweb request-handlers filters))
