(ns try-reagent.macros)

(defmacro <? [expr]
  `(let [v# (cljs.core.async/<! ~expr)]
     (println "v#" v#)
     (if (instance? js/Error v#)
       (throw v#)
       v#)))

