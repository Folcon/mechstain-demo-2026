(ns inkstain.systems.grid
  (:require [inkstain.utils :as utils]))



;; TODO(#1): extend to 3D (depth)
(defn make-grid [width height]
  {:width width :height height
   :terrain (vec (repeat (* width height) :soil))
   :walkable (vec (repeat (* width height) true))})

(defn clamp [grid [x y]]
  [(utils/clamp x 0 (dec (:width grid)))
   (utils/clamp y 0 (dec (:height grid)))])

(defn cell-idx [x y width]
  (+ x (* y width)))

(defn terrain-at [m x y]
  (get (:terrain m) (cell-idx x y (:width m)) :soil))

(defn walkable? [m x y]
  (and (>= x 0) (>= y 0) (< x (:width m)) (< y (:height m))
    (get (:walkable m) (cell-idx x y (:width m)) false)))

(defn set-walkable [m x y v]
  (assoc-in m [:walkable (cell-idx x y (:width m))] v))

(defn set-terrain [m x y terrain]
  (assoc-in m [:terrain (cell-idx x y (:width m))] terrain))

(defn scatter-water [grid n]
  (reduce (fn [g _]
            (let [x (rand-int (:width g))
                  y (rand-int (:height g))]
              (-> g
                (set-terrain x y :water)
                (set-walkable x y false))))
    grid (range n)))
