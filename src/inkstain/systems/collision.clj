(ns inkstain.systems.collision
  (:require [inkstain.systems.grid :as grid]))



;; Going to treat these a bit like boids and do separation

;; spatial hash
;; tiles per bucket, should be tuned to ~2x the largest unit radius
(def cell-size 4.0)

(defn cell-key [[x y]]
  [(int (Math/floor (/ x cell-size)))
   (int (Math/floor (/ y cell-size)))])

(defn build-spatial-hash
  "map of {[cx cy] [entity ...]}"
  [entities]
  (persistent!
    (reduce
      (fn [acc ent]
        (let [k (cell-key (:pos ent))]
          (assoc! acc k (conj (get acc k []) ent))))
      (transient {})
      entities)))

(defn nearby-entities [spatial-hash pos]
  ;; check the 3x3 neighborhood of cells around pos
  (let [[cx cy] (cell-key pos)]
    (into []
      (mapcat #(get spatial-hash % []))
      (for [dx [-1 0 1] dy [-1 0 1]]
        [(+ cx dx) (+ cy dy)]))))

;; separation steering to push units apart
(def unit-radius 0.4)         ;; in tiles, tune as needed
(def separation-radius 1.0)   ;; start pushing at this distance
(def separation-strength 8.0) ;; tiles/sec^2, how hard units push apart

(defn separation-force
  "compute repulsion vector [fx fy] for entity from neighbors"
  [entity neighbors]
  (let [[px py] (:pos entity)
        id (:id entity)]
    (reduce
      (fn [[fx fy] other]
        (if (= id (:id other))
          [fx fy]
          (let [[ox oy] (:pos other)
                dx (- px ox)
                dy (- py oy)
                dist-sq (+ (* dx dx) (* dy dy))
                min-dist separation-radius]
            (if (and (> dist-sq 0.001) (< dist-sq (* min-dist min-dist)))
              (let [dist (Math/sqrt dist-sq)
                    ;; force falls off linearly, strongest when overlapping
                    overlap (- min-dist dist)
                    scale (/ (* separation-strength overlap) dist)]
                [(+ fx (* dx scale))
                 (+ fy (* dy scale))])
              [fx fy]))))
      [0.0 0.0]
      neighbors)))

(defn apply-separation [entities dt spatial-hash]
  (mapv
    (fn [ent]
      (let [neighbors (nearby-entities spatial-hash (:pos ent))
            [fx fy] (separation-force ent neighbors)
            [px py] (:pos ent)]
        (if (and (zero? fx) (zero? fy))
          ent
          (assoc ent :pos [(+ px (* fx dt))
                           (+ py (* fy dt))]))))
    entities))

;; prevent units from being pushed into invalid terrain
(defn clamp-to-walkable
  "if separation pushed entity onto unwalkable terrain, revert to old pos"
  [entity old-pos grid]
  (let [[x y] (:pos entity)
        ix (int (Math/round ^double x))
        iy (int (Math/round ^double y))]
    (if (grid/walkable? grid ix iy)
      entity
      (assoc entity :pos old-pos))))
