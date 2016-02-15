(ns json-demo.handler
  (:require [compojure.core :refer :all]
            [compojure.route :refer [not-found]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json
             :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :refer [response redirect]]))

(defn respond-to-json [s]
  (println "post-parameters: " s)
  (response {:main-character2 "Huck"
              :main-character1 "Tom"}))

(defroutes app-routes
  (POST "/a" [s] (respond-to-json s))

  (GET "/" []
       (redirect "/postjson.html"))

  (not-found (str "no mathching route found\n"
                  "Try <a href='/postjson.html'>this</a>")))

(def app
  (-> app-routes
      (wrap-params)
      (wrap-resource "public")
      (wrap-json-body)
      (wrap-json-response)))
