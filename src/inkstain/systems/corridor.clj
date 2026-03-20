(ns inkstain.systems.corridor
  (:require [inkstain.systems.grid :as grid]))



(defn walkable-line?
  "check if a straight line between two points crosses only walkable tiles
   sampling at ~0.5 tile intervals along the line"
  [grid [x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))
        ;; number of samples, at least 2 (the endpoints)
        steps (max 2 (int (Math/ceil (/ dist 0.5))))
        step-x (/ dx (double steps))
        step-y (/ dy (double steps))]
    (loop [i 0]
      (if (> i steps)
        true ;; all samples walkable
        (let [sx (+ x1 (* i step-x))
              sy (+ y1 (* i step-y))
              tx (Math/round ^double sx)
              ty (Math/round ^double sy)]
          (if (grid/walkable? grid tx ty)
            (recur (inc i))
            false))))))

(defn smooth-path
    "removes redundant waypoints by skipping those with clear line-of-sight
     given path [A B C D E], if A can see C directly then B is redundant
     keep scanning forward to find the furthest visible point"
    [grid path]
    (let [path (vec path)
          n (count path)]
      (if (< n 3)
        ;; nothing to smooth
        path
        (loop [current 0
               result [(path 0)]]
          (if (>= current (dec n))
            ;; reached the end
            (if (not= (peek result) (path (dec n)))
              (conj result (path (dec n)))
              result)
            ;; scan forward, find the furthest point visible from current
            (let [furthest
                  (loop [probe (+ current 2)]
                    (cond
                      ;; past the end, last reachable was one before
                      (>= probe n)
                      (dec probe)

                      ;; can we see this probe from current?
                      (walkable-line? grid (path current) (path probe))
                      (recur (inc probe))

                      ;; can't see probe, so the furthest was one before
                      :else
                      (dec probe)))]
              (recur furthest
                (conj result (path furthest)))))))))

(comment
  (let [grid (grid/make-grid 10 10)
        ;; [[5 3] [9 5]]
        _ (println (smooth-path grid [[5 3] [6 4] [7 5] [8 5] [9 5]]))
        grid' (grid/set-walkable grid 7 4 false)]
    ;; blocks
    ;; [[5 3] [7 5] [9 5]]
    (println (smooth-path grid' [[5 3] [6 4] [7 5] [8 5] [9 5]]))))
