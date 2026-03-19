(ns inkstain.systems.movement)



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

(defn facing-from-delta
  "determine facing direction from movement delta"
  [dx dy]
  (if (> (Math/abs (double dx)) (Math/abs (double dy)))
    (if (pos? dx) :east :west)
    (if (pos? dy) :north :south)))

(defn step-movement [peep dt]
  (if (and (= (:state peep) :moving) (seq (:path peep)))
    (let [[next-node & remaining] (:path peep)
          [nx ny] next-node
          [px py] (:pos peep)
          dx (- nx px) dy (- ny py)
          facing (if (and (zero? dx) (zero? dy))
                   (:facing peep :south)
                   (facing-from-delta dx dy))
          job-type (get-in peep [:job :type])
          urgency (get job-locomotion job-type :run)
          speed (* (:max-speed peep) (get locomotion-speed urgency 1.0))
          [new-x new-y arrived?] (move-toward [px py] [nx ny] speed dt)]
      (if arrived?
        (if (seq remaining)
          (assoc peep :pos [new-x new-y] :path (vec remaining) :facing facing)
          (assoc peep :pos [new-x new-y] :path [] :state :idle :facing facing))
        (assoc peep :pos [new-x new-y] :facing facing)))
    peep))


(def chassis-movement
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
  [chassis-props speed]
  (let [{:keys [turn-rate-rest turn-rate-speed
                turn-speed-threshold max-speed]} chassis-props
        ;; lerp factor, 0 at rest, 1 at max speed
        t (if (> max-speed turn-speed-threshold)
            (min 1.0 (/ (max 0 (- speed turn-speed-threshold))
                       (- max-speed turn-speed-threshold)))
            0.0)]
    (Math/toRadians (+ (* (- 1.0 t) turn-rate-rest)
                      (* t turn-rate-speed)))))

(defn movement-chassis [entity]
  (if (:mech entity)
    (get-in entity [:mech :chassis] :standard)
    ;; add support for leaving the mech
    :infantry))

(defn step-physics [entity dt]
  (let [chassis-type (movement-chassis entity)
        {:keys [max-speed acceleration deceleration] :as props} (chassis-movement chassis-type)

        speed (:speed entity 0.0)
        heading (:heading entity 0.0)
        target-speed (:target-speed entity 0.0)
        target-heading (:target-heading entity heading)

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
        new-y (+ py (* vy dt))]
    (assoc entity
      :pos [new-x new-y]
      :heading new-heading
      :speed new-speed)))
