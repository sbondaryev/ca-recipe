(ns rest-media-demo.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [liberator.core :refer [resource defresource]]
            [liberator.representation :refer [render-map-generic]]
            [hiccup.core :refer [html]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure.data.json :as json]))

(def default-media-types
  ["application/json"
   "text/plain"
   "text/html"])

(defmethod render-map-generic "application/json" [data context]
  (json/write str (conj (:links data) (:properties data) )))

(defmethod render-map-generic "text/html" [data context]
  (html [:div
         [:h1 (-> data :class first)]
         [:dl
          (mapcat (fn [[key value]] [[:dt key] [:dd value]])
                  (:properties data))]]))

(defrecord Coffee [name price])

(defroutes app-routes
  (GET "/" [] "Hello World")
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))
