(ns inkstain.systems.corridor
  (:require [inkstain.systems.grid :as grid]
            [inkstain.math :refer [distance]]
            [inkstain.systems.movement :as movement]))



(def drive-train-doctrine
  {:infantry  {:lookahead-scale     1.0   ;; multiplier
               :curvature-brake     0.5   ;; how aggressively to slow for turns [0-1]
               :path-tolerance      2.0}  ;; tiles off-path before correcting

   :standard  {:lookahead-scale     1.0
               :curvature-brake     0.7
               :path-tolerance      1.5}

   :charger   {:lookahead-scale     1.5   ;; looks far ahead
               :curvature-brake     0.9   ;; brakes hard for turns, prefer full stop and turn
               :path-tolerance      1.0}  ;; stays close to corridor

   :dasher    {:lookahead-scale     0.8   ;; shorter lookahead, relying on stopping to turn
               :curvature-brake     0.5   ;; moderate braking, can decel fast anyway
               :path-tolerance      2.5}}) ;; wide tolerance, able to cut corners freely


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

(defn nearest-segment
  "find the nearest point on the polyline path to pos"
  [path pos]
  (let [n (count path)]
    (if (< n 2)
      {:segment-idx 0 :projection (first path) :distance 0.0 :t 0.0}
      ;; for segment from A to B, projecting point P
      (let [[px py] pos]
        (loop [idx 0
               best-idx 0
               best-dist Double/MAX_VALUE
               best-proj nil
               ;; distance along the segment
               best-t 0.0]
          (if (>= idx (dec n))
            {:segment-idx best-idx
             :projection best-proj
             :distance best-dist
             :t best-t}
            (let [[ax ay] (path idx)
                  [bx by] (path (inc idx))
                  abx (- bx ax)
                  aby (- by ay)
                  apx (- px ax)
                  apy (- py ay)
                  ab-sq (+ (* abx abx) (* aby aby))
                  t (if (< ab-sq 0.0001)
                      0.0
                      (max 0.0 (min 1.0 (/ (+ (* apx abx) (* apy aby))
                                           ab-sq))))
                  proj-x (+ ax (* t abx))
                  proj-y (+ ay (* t aby))
                  dist (distance [px py] [proj-x proj-y])
                  ;; prefer later segment when distances are close
                  ;;   prevents snapping back to an earlier segment
                  better? (or (< dist (- best-dist 0.1))
                              (and (< dist (+ best-dist 0.1))
                                   (> idx best-idx)))]
              (recur (inc idx)
                     (if better? idx best-idx)
                     (if better? dist best-dist)
                     (if better? [proj-x proj-y] best-proj)
                     (if better? t best-t)))))))))

(comment
  (let [path [[0 0] [4 0] [4 4]]
        pos [3.5 1.0]]
    (nearest-segment path pos)))

(defn lookahead-target
  "walk forward dist tiles along the path from segment-idx at parameter t
  (distance along that segment)"
  [path segment-idx t dist]
  (let [n (count path)]
    (if (< n 2)
      (first path)
      (loop [remaining dist
             idx segment-idx
             frac t]
        (if (>= idx (dec n))
          ;; past end of path, return last waypoint
          (peek path)
          (let [a (path idx)
                b (path (inc idx))
                seg-len (distance a b)
                remaining-on-seg (* (- 1.0 frac) seg-len)]
            (if (<= remaining remaining-on-seg)
              ;; target falls on this segment
              (let [new-t (if (< seg-len 0.0001)
                            frac
                            (+ frac (/ remaining seg-len)))
                    [ax ay] a
                    [bx by] b]
                [(+ ax (* new-t (- bx ax)))
                 (+ ay (* new-t (- by ay)))])
              ;; keep walking to next segment
              (recur (- remaining remaining-on-seg)
                (inc idx)
                0.0))))))))

(comment
  (let [path [[0 0] [4 0] [4 4]]
        lookahead-dist 2.0
        ;; [4.0 3.0]
        _ (println (lookahead-target path 1 0.25 lookahead-dist))
        lookahead-dist 5.0]
    ;; [4, 4]
    (println (lookahead-target path 1 0.25 lookahead-dist))))

(defn curvature-ahead
  "estimate turn angle (radians, 0 to pi) at the junction after segment-idx
   returns 0.0 if on the last segment"
  [path segment-idx]
  (let [n (count path)]
    (if (>= segment-idx (- n 2))
      0.0
      (let [[ax ay] (path segment-idx)
            [bx by] (path (inc segment-idx))
            [cx cy] (path (+ segment-idx 2))
            heading-1 (Math/atan2 (- by ay) (- bx ax))
            heading-2 (Math/atan2 (- cy by) (- cx bx))
            diff (- heading-2 heading-1)
            ;; normalise to [-pi pi]
            diff (let [d (mod diff (* 2 Math/PI))]
                   (if (> d Math/PI) (- d (* 2 Math/PI)) d))]
        (Math/abs ^double diff)))))

(defn query-corridor
  "given a smoothed path and unit position return a corridor"
  [path pos lookahead-dist]
  (when (>= (count path) 2)
    (let [{:keys [segment-idx t projection distance]} (nearest-segment path pos)
          target (lookahead-target path segment-idx t lookahead-dist)
          curvature (curvature-ahead path segment-idx)]
      {:projection projection
       :segment-idx segment-idx
       :target target
       :curvature curvature
       :distance-to-path distance})))


(defn step-corridor-steering
  "kinematically-aware path following, we read the smoothed path as a corridor and outputs heading+speed appropriate for the entity's chassis"
  [entity dt]
  (let [path (:path entity)]
    (if (or (not= (:state entity) :moving)
          (nil? path)
          (< (count path) 2))
      ;; not moving or no valid path
      entity

      (let [drive-train-type (get-in entity [:mech :drive-train] :standard)
            doctrine (get drive-train-doctrine drive-train-type
                       (drive-train-doctrine :standard))
            {:keys [lookahead-scale curvature-brake path-tolerance]} doctrine

            ;; chassis properties
            drive-train-props (movement/movement-drive-train entity)
            {:keys [max-speed deceleration]} drive-train-props
            speed (or (:speed entity) 0.0)

            ;; lookahead from chassis kinematics, ie how far do we need to look forwards to get enough warning that we need to turn
            turn-rate (movement/get-turn-rate drive-train-props speed)
            base-lookahead 1.5
            kinematic-lookahead (if (> turn-rate 0.01)
                                  (/ speed turn-rate)
                                  0.0)
            lookahead-dist (+ base-lookahead (* kinematic-lookahead lookahead-scale))

            ;; query corridor, where's our target?
            corridor (query-corridor path (:pos entity) lookahead-dist)]

        (if (nil? corridor)
          entity

          (let [[px py] (:pos entity)
                [tx ty] (:target corridor)

                ;; desired heading toward lookahead target
                desired-heading (Math/atan2 (- ty py) (- tx px))

                ;; drift correction
                ;;   if too far off-path, steer back toward projection
                drift (:distance-to-path corridor)
                desired-heading' (if (> drift path-tolerance)
                                   ;; blend toward the projection point to get back on track
                                   (let [[prx pry] (:projection corridor)
                                         correction-heading (Math/atan2 (- pry py) (- prx px))
                                         ;; blend factor, 0 at tolerance, 1 at 2x tolerance
                                         blend (min 1.0 (/ (- drift path-tolerance) path-tolerance))
                                         ;; weighted average of desired and correction headings
                                         diff (movement/angle-diff desired-heading correction-heading)
                                         blended (+ desired-heading (* blend diff))]
                                     (movement/normalise-angle blended))
                                   desired-heading)

                ;; curvature braking, slow down for upcoming turns
                curvature-factor (- 1.0 (* (/ (:curvature corridor) Math/PI) curvature-brake))

                ;; arrival braking, slow down the closer we are to the target
                [fx fy] (peek path)
                dx (- fx px) dy (- fy py)
                dist-to-end (Math/sqrt (+ (* dx dx) (* dy dy)))
                arrival-factor (if (> deceleration 0.01)
                                 (let [brake-dist (/ (* speed speed)
                                                    (* 2.0 deceleration))]
                                   (min 1.0 (/ dist-to-end (max brake-dist 0.5))))
                                 1.0)

                ;; locomotion urgency
                job-type (get-in entity [:job :type])
                urgency (get movement/job-locomotion job-type :run)
                urgency-mul (get movement/locomotion-speed urgency 1.0)

                ;; final desired speed
                desired-speed (* max-speed urgency-mul
                                (min curvature-factor arrival-factor))

                ;; are we there yet?
                arrival-dist (max 0.5 (* speed dt 2))]

            (if (<= dist-to-end arrival-dist)
              (assoc entity :state :idle :target-speed 0.0 :path nil)
              (assoc entity
                :target-heading desired-heading'
                :target-speed desired-speed))))))))
