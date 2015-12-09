(ns ca-recipe.tnywb.core
  (:require [clojure.string :as str])
  (:import (com.mblinn.oo.tinyweb RenderingException ControllerException)))

(defn- render [view model]
  (try
    (view model)
    (catch Exception e (throw (RenderingException. e)))))
