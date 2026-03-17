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


(defn random-edge-pos [grid]
  (let [{:keys [width height]} grid
        edge (rand-int 4)]
    (case edge
      0 [(rand-int width) 0]            ;; top
      1 [(rand-int width) (dec height)] ;; bottom
      2 [0 (rand-int height)]           ;; left
      3 [(dec width) (rand-int height)] ;; right
      ,)))

(defn random-spawn-pos [player-pos grid]
  (let [[px py] player-pos
        {:keys [width height]} grid
        radius 20
        angle (* (rand) 2.0 Math/PI)
        x (int (+ px (* radius (Math/cos angle))))
        y (int (+ py (* radius (Math/sin angle))))
        x (max 0 (min (dec width) x))
        y (max 0 (min (dec height) y))]
    (if (walkable? grid x y)
      [x y]
      ;; fallback: try a few more times
      (let [[x y] (first (filter (fn [[x y]] (walkable? grid x y))
                           (for [_ (range 10)]
                             (let [a (* (rand) 2.0 Math/PI)]
                               [(max 0 (min (dec width) (int (+ px (* radius (Math/cos a))))))
                                (max 0 (min (dec height) (int (+ py (* radius (Math/sin a))))))]))))]
        (or (when (and x y) [x y])
          [(int px) (int py)])))))
