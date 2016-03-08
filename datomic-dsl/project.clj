(defproject datomic-dsl "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.datomic/datomic-free "0.9.5350" :exclusions [joda-time]]]
  :main ^:skip-aot datomic-dsl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
