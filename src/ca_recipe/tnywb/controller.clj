(ns ca-recipe.tnywb.controller
  (:import (com.mblinn.oo.tinyweb HttpRequest HttpRequest$Builder)))
(defn test-controller [http-request] {:name (.getBody http-request)})
(def test-builder (HttpRequest$Builder/newBuilder))
(def test-http-request (.. test-builder (body "Some text") (path "/say-hello") build)) (defn test-controller-with-map [http-request]
                                                                                    {:name (http-request :body)})
