(ns cljs-rest-client-server-demo.handler
  (:require [ring.util.response :as ring-res]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.middleware.resource :as resources]
            [ring.middleware.params :as params]
            [compojure.core :refer :all]
            [ring.middleware.file-info :refer :all]))

(defroutes app-routes
  (PUT "/:id" {params :params}
       (str "put colled with params: " params))
  (POST "/:id" {params :params}
        (str "post called with params: " params))
  (route/resources "/")
  (route/not-found
   "<a href='/index.html'>Try it</a>"))

(def app
  (-> app-routes
      (params/wrap-params)
      handler/api
      (resources/wrap-resource "public")
      (wrap-file-info)))
        

