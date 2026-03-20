(ns inkstain.systems.movement
  (:require [inkstain.systems.grid :as grid]))



(defn heading->facing [heading]
  (let [deg (mod (Math/toDegrees heading) 360)]
    (cond
      (or (< deg 45) (>= deg 315)) :east
      (< deg 135) :south    ;; +y is south
      (< deg 225) :west
      :else :north)))

(def drive-train-movement
  {;; peep not in a mech
   :infantry  {:max-speed 2.5
               :acceleration 20.0
               :deceleration 20.0
               :turn-rate-rest 720
               :turn-rate-speed 720
               :turn-speed-threshold 0.0}
   :standard {:max-speed 4.0
              :acceleration 12.0     ;; high = responsive
              :deceleration 12.0
              :turn-rate-rest 360    ;; degrees/sec, instant at rest
              :turn-rate-speed 270   ;;   still good at speed
              :turn-speed-threshold 1.0} ;; below this speed, use rest turn rate
   ,})

(defn normalise-angle
  "normalise angle to [-pi, pi]"
  [a]
  (let [a (mod a (* 2 Math/PI))]
    (if (> a Math/PI) (- a (* 2 Math/PI)) a)))

(defn angle-diff [a b]
  (normalise-angle (- b a)))

(defn get-turn-rate
  "turn rate in radians/sec based on current speed"
  [drive-train-props speed]
  (let [{:keys [turn-rate-rest turn-rate-speed
                turn-speed-threshold max-speed]} drive-train-props
        ;; lerp factor, 0 at rest, 1 at max speed
        t (if (> max-speed turn-speed-threshold)
            (min 1.0 (/ (max 0 (- speed turn-speed-threshold))
                       (- max-speed turn-speed-threshold)))
            0.0)]
    (Math/toRadians (+ (* (- 1.0 t) turn-rate-rest)
                      (* t turn-rate-speed)))))

(defn movement-drive-train [entity]
  (if (:mech entity)
    (get-in entity [:mech :drive-train] :standard)
    ;; add support for leaving the mech
    :infantry))

(defn step-physics [entity grid dt]
  (let [drive-train-type (movement-drive-train entity)
        {:keys [max-speed acceleration deceleration] :as props} (drive-train-movement drive-train-type)

        speed (or (:speed entity) 0.0)
        heading (or (:heading entity) 0.0)
        target-speed (or (:target-speed entity) 0.0)
        target-heading (or (:target-heading entity) heading)

        ;; turn toward target heading
        max-turn (* (get-turn-rate props speed) dt)
        diff (angle-diff heading target-heading)
        turn (cond
               (> (Math/abs ^double diff) max-turn)
               (* (Math/signum ^double diff) max-turn)
               :else diff)
        new-heading (normalise-angle (+ heading turn))

        ;; accelerate
        speed-diff (- target-speed speed)
        accel (if (pos? speed-diff) acceleration deceleration)
        max-speed-change (* accel dt)
        new-speed (if (> (Math/abs ^double speed-diff) max-speed-change)
                    (+ speed (* (Math/signum ^double speed-diff) max-speed-change))
                    target-speed)
        new-speed' (max 0.0 (min max-speed new-speed))

        ;; update position from heading + speed
        [px py] (:pos entity)
        vx (* new-speed' (Math/cos new-heading))
        vy (* new-speed' (Math/sin new-heading))
        new-x (+ px (* vx dt))
        new-y (+ py (* vy dt))

        new-pos (let [rx (Math/round ^double new-x)
                      ry (Math/round ^double new-y)]
                 (cond
                   ;; target walkable, keep going
                   (grid/walkable? grid rx ry) [new-x new-y]
                   ;; slide along x or y
                   (grid/walkable? grid (Math/round ^double px) ry) [px new-y]
                   (grid/walkable? grid rx (Math/round ^double py)) [new-x py]
                   ;; stop
                   ;; TODO: Damage for slamming to a stop?
                   :else [px py]))

        ;; set facing for animation
        facing (heading->facing new-heading)]
    (assoc entity
      :pos new-pos
      :heading new-heading
      :speed new-speed
      :facing facing)))

(def locomotion-speed
  "speed multiplier per locomotion urgency"
  {:amble   0.33    ;; 3x slower - wandering
   :walk    0.5     ;; 2x slower - normal slow activity
   :run     1.0     ;; default
   :redline 1.33})  ;; 25% faster - fleeing, extra heat

(def job-locomotion
  "default locomotion urgency per job type"
  {:wander        :amble
   :haul          :walk
   :equip         :walk
   :melee-attack  :run
   :ranged-attack :run
   :flee          :redline})

(defn move-toward [[x y] [tx ty] speed dt]
  (let [dx (- tx x) dy (- ty y)
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))
        step (* speed dt)]
    (if (<= dist step)
      [tx ty true]
      (let [scale (/ step dist)]
        [(+ x (* dx scale))
         (+ y (* dy scale))
         false]))))

(defn step-movement [peep dt]
  (if (and (= (:state peep) :moving) (seq (:path peep)))
    (let [[next-node & remaining] (:path peep)
          [nx ny] next-node
          [px py] (:pos peep)
          dx (- nx px) dy (- ny py)
          dist (Math/sqrt (+ (* dx dx) (* dy dy)))

          ;; arrival threshold, scale with speed so fast units don't overshoot
          speed (or (:speed peep) 0.0)
          arrival-dist (max 0.5 (* speed dt 2))]
      (if (<= dist arrival-dist)
        ;; close enough so we pop this waypoint
        (if (seq remaining)
          (let [[nnx nny] (first remaining)
                new-heading (Math/atan2 (- nny py) (- nnx px))]
            (assoc peep :path (vec remaining) :target-heading new-heading))
          ;; last waypoint so we stop
          (assoc peep :path [] :state :idle :target-speed 0.0))

        ;; not there yet so aim at waypoint
        (let [target-heading (Math/atan2 dy dx)

              job-type (get-in peep [:job :type])
              ;; TODO: Add combat based urgency as well to flesh out combat
              urgency (get job-locomotion job-type :run)
              max-speed (or (-> peep :mech :drive-train drive-train-movement :max-speed)
                          (:max-speed peep))
              effective-speed (* max-speed (get locomotion-speed urgency 1.0))

              ;; check if we need to slow down
              needs-slow? (if (empty? remaining)
                            ;; last waypoint so we stop
                            true
                            ;; check angle to nnext waypoint
                            (let [[nnx nny] (first remaining)
                                  next-heading (Math/atan2 (- nny ny) (- nnx nx))
                                  turn-angle (Math/abs (double (angle-diff target-heading next-heading)))]
                              ;; slow down if turning more than ~45 degrees
                              (> turn-angle (/ Math/PI 4))))

              speed-frac (if needs-slow?
                           (min 1.0 (/ dist 2.0))
                           1.0)]
          (assoc peep
            :target-heading target-heading
            :target-speed (* effective-speed speed-frac)))))
    peep))
