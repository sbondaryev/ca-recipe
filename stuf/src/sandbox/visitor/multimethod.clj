(ns sandbox.visitor.multimethod)
;; expression problem

;; initial domain
(defmulti perimeter (fn [shape] (:shape-name shape)))

(defmethod perimeter :circle [circle]
  (* 2 Math/PI (:radius circle)))
(defmethod perimeter :rectangle [rectangle]
  (+ (* 2 (:width rectangle)) (* 2 (:height rectangle))))

(def some-shapes [{:shape-name :circle :radius 4}
                  {:shape-name :rectangle :width 2 :height 2}])

;; extend method
(defmulti area (fn [shape] (:shape-name shape)))

(defmethod area :circle [circle]
  (* Math/PI (:radius circle) (:radius circle)))
(defmethod area :rectangle [rectangle]
  (* (:width rectangle) (:height rectangle)))

;; extend type
(defmethod perimeter :square [square]
  (* 4 (:side square)))
(defmethod area :square [square]
  (* (:side square) (:side square)))

(def more-shapes (conj some-shapes
                       {:shape-name :square :side 4}))

