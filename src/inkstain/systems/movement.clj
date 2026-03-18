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
