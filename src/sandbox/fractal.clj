(ns sandbox.fractal
  (:import (java.awt image.BufferedImage Color)
           (javax.imageio ImageIO)
           (java.io File)))

(defn pascal-row-step [yield pascal-row]
  {:pre [(> (get  pascal-row 0) 0)]}
  (vec (map #(apply yield %) (partition 2 1 (concat [0] pascal-row [0])))))

(defn pascal-rows [yield row-number]
  (loop [nb 0
         result []
         latest-result [1]]
;;=> We'll loop using pascal-row-step,
;;=> keeping track of the last
;;computed line at each step of the recursion.
    (if (<= nb row-number)
;;=> the counter did not still reach the end
      (recur (inc nb)
             (conj result latest-result)
             (pascal-row-step yield latest-result))
;;=> We recur incrementing the counter, feeding the new line to
;; result and keeping track of the last computed line.
      result)))
;;=> end of the recursion, emitting result.

(defn even-odd-yield
  [n1 n2]
  (mod (+ n1 n2) 2))

(def gr-triangles (partial pascal-rows even-odd-yield))

(defn draw [size]
  (let [img (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
;;=> Creating img as a Buffered Image
        plot-rows (gr-triangles size)
;;=> computing the triangle of 0 and 1
        plots (for [x (range 0 size) y (range 0 x)]
                (if (= 1 (get (get plot-rows x) y))
                  [x y]))
;;=> we save the positions holding 1 in vectors. As the structure
;; is triangular;
;; the first counter, "x" goes up to "size", and the second one,
;; "y",
;;    goes up to "x"
        gfx (.getGraphics img)]
;;=> we get the graphics component, where to draw from the Java
;; Object.
    (.setColor gfx Color/WHITE)
    (.fillRect gfx 0 0 size size )
;;=> we set a white background for the image.
    (.setColor gfx Color/BLACK)
;;=> We set the pen color to black again
    (doseq [p (filter (comp not nil?)  plots)]
      (.drawLine gfx
                 (get p 0)
                 (get p 1)
                 (get p 0)
                 (get p 1)))
;;=> We plot, by drawing a line from and to the same point.
    (ImageIO/write img "png"
                   (File. "result.png"))))
;;=> and we save the image as a png in this location.
;; Be sure to set a correct one when running on your machine !
