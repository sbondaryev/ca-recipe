(ns sandbox.stack
  (:require [instaparse.core :as insta])
  (:require [clojure.zip :as z])
  (:require [clojure.pprint :refer :all])
  (:require [clojure.walk :as walk]))

(def p pprint)

(def language
         "S =  ((FN-CALL|FN-DECL) <FN-SEP>)*
          FN-CALL = <optional-whitespace> ID <optional-whitespace>
                    <left-paren> PARAMS-LIST <right-paren>
          PARAMS-LIST = <optional-whitespace> (ID|FN-CALL)
                                   (<optional-whitespace> <PARAMS-SEP>
                                    <optional-whitespace> (ID|FN-CALL))*
          FN-DECL = <optional-whitespace> 'decl-fn'
                            <whitespace> ID <optional-whitespace>
                             <left-paren> ARGS-LIST <right-paren>
                            <optional-whitespace>
                            <left-curly>  FN-DECL-BODY <right-curly>
          ARGS-LIST = <optional-whitespace> ID
                               (<optional-whitespace> <PARAMS-SEP>
                                <optional-whitespace> ID)*
          FN-DECL-BODY = (FN-CALL <FN-SEP>)*
          left-paren = '('
          right-paren = ')'
          left-curly = '{'
          right-curly = '}'
          ID = #'[a-zA-Z0-9]+'
          whitespace = #'\\s+'
          optional-whitespace = #'\\s*'
          FN-SEP = <optional-whitespace> ';' <optional-whitespace>
          PARAMS-SEP = <optional-whitespace> ',' <optional-whitespace>")

(defn gen-program [parser program]
  (into [] (insta/transform
            {:S (fn [ & args] args)
             :FN-CALL (fn [fn-id params] [:FN-CALL
                                          (fn-id 1)
                                          params])
             :PARAMS-LIST (fn[& params] (into [] params) )
             :FN-DECL (fn [_ decl-fn-id  args body] [:FN-DECL
                                                     (decl-fn-id 1)
                                                     args body])
             :ARGS-LIST (fn [& args] (into [] args))
             :FN-DECL-BODY (fn [& body] (into [] body))}
            (parser program))))

(defn get-fn-decls [program]
  (->> program
       (filter #(= :FN-DECL (get % 0)))
       (into [])))

(defn get-instructions [program]
  (->> program
       (filter #(not= :FN-DECL (get % 0)))
       (into [])))

(defn get-fn-id-decl [fn-decls fn-id]
  (->> fn-decls
       (filter #(= (get % 1)
                   fn-id))
       (first)))

(defn call-fn [fn-decl fn-call]
  (let [decl-args-list (fn-decl 2)
        decl-body (fn-decl 3)
        fn-call-params (fn-call 2)]
    (if (not (= (count decl-args-list) (count fn-call-params)))
      [:arity-error-in-calling (fn-decl 1 )]
      (let [replacement-map (zipmap decl-args-list fn-call-params)]
        (walk/postwalk-replace replacement-map decl-body)))))

(defn expand-to-primitive-calls [program]
  (let  [fn-decls (get-fn-decls program)
         instructions (get-instructions program)
         zpr (z/vector-zip instructions)]
    (loop [result instructions
           loc (-> zpr z/down)]
      (if (-> loc z/end?)
        result
        (let [current-node (-> loc z/node)]
          (if (= (get current-node 0 :FN-CALL))
            (if-let [the-decl (get-fn-id-decl fn-decls (get current-node 1))]
              (recur (walk/postwalk-replace {(-> loc z/node)
                                             (call-fn the-decl
                                                      current-node)}
                                            result )
                     (->  loc z/next))
              (recur result (-> loc z/next)))
            (recur result (-> loc z/next))))))))

(defn call-stack [call]
  (let [zpr (z/vector-zip call)]
    (loop [result []
           loc (-> zpr z/down)]
      (if (-> loc z/end?)
        result
        (let [current-node (-> loc z/node)]
          (recur (if (and
                      (not (vector? current-node))
                      (not= :FN-CALL current-node)
                      (not= :ID current-node))
                   (conj result {(-> loc z/left z/node) current-node})
                   result)
                 (-> loc z/next)))))))

(defn program-call-stack [prog]
  (->> (expand-to-primitive-calls prog)
       (mapv call-stack)))

