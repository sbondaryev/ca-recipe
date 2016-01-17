(ns sandbox.fractal
  (:import (java.awt image.BufferedImage Color)
           (javax.imageio ImageIO)
           (java.io File)))

(defn pascal-row-step [yield pascal-row]
  {:pre [(> (get  pascal-row 0) 0)]}
  (vec (map #(apply yield %) (partition 2 1 (concat [0] pascal-row [0])))))

(defn pascal-rows [yield row-number]
  (vec (take row-number (iterate (partial pascal-row-step yield) [1]))))

(defn even-odd-yield
  [n1 n2]
  (mod (+ n1 n2) 2))

(def gr-triangles (partial pascal-rows even-odd-yield))

(defn draw [size]
  (let [img (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        plot-rows (gr-triangles size)
        plots (for [x (range 0 size) y (range 0 x)]
                (if (= 1 (get (get plot-rows x) y))
                  [x y]))
        gfx (.getGraphics img)]
    (.setColor gfx Color/WHITE)
    (.fillRect gfx 0 0 size size )
    (.setColor gfx Color/BLACK)
    (doseq [[x y] (filter (comp not nil?)  plots)]
      (.drawLine gfx x y x y))
    (ImageIO/write img "png" (File. "result.png"))))

