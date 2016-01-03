(ns sandbox.dsl.shell
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(defn- print-output [output]
  (println (str "Exit Code: " (:exit output)))
  (if-not (str/blank? (:out output)) (println (:out output)))
  (if-not (str/blank? (:err output)) (println (:err output)))
  output)

(defn command [command-str]
  (let [command-parts (str/split command-str #"\s+")]
    (fn
      ([] (print-output (apply shell/sh command-parts)))
      ([{old-out :out}]
       (print-output (apply shell/sh (concat command-parts [:in old-out])))))))

(defn pipe [commands]
  (apply comp (reverse commands)))


(def pwd (command "pwd"))
(def ls (command "ls"))
(def grep-readme-from-ls
  (pipe
   [(command "ls")
    (command "grep README")]))
