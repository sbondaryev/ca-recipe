(ns sandbox.customflow.macrotiming)

(defn time-run [to-time]
  (let [start (System/currentTimeMillis)]
    (to-time)
    (- (System/currentTimeMillis) start)))

(defmacro avg-time [times to-time]
  `(let [total-time#
         (apply + (for [_# (range ~times)] (time-run (fn [] ~to-time))))]
     (float (/ total-time# ~times))))
