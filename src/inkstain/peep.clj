(ns inkstain.peep
  (:require [inkstain.state :as state]))



(def chassis-size
  {:light   {:hp  60 :max-hp  60 :size 0.3 :attack-damage  6 :attack-range 1.5}
   :medium  {:hp 100 :max-hp 100 :size 0.4 :attack-damage 10 :attack-range 1.5}
   :heavy   {:hp 160 :max-hp 160 :size 0.5 :attack-damage 16 :attack-range 1.5}
   :assault {:hp 220 :max-hp 220 :size 0.6 :attack-damage 22 :attack-range 1.5}})

(def chassis-size-speed-modifier
  "speed multiplier per chassis size"
  {:light   {:speed-mul 1.3  :accel-mul 1.2  :turn-mul 1.2}
   :medium  {:speed-mul 1.0  :accel-mul 1.0  :turn-mul 1.0}
   :heavy   {:speed-mul 0.75 :accel-mul 0.8  :turn-mul 0.8}
   :assault {:speed-mul 0.6  :accel-mul 0.6  :turn-mul 0.6}})

(defn random-chassis []
  (rand-nth (keys chassis-size)))

(defn make-peep [pos faction]
  {:id (state/next-id)
   :pos pos              ;; grid position [x y]
   :max-speed 4.0        ;; tiles per second as a peep

   ;; mech speed etc comes from drive-train
   :mech {:chassis :medium           ;; :light :medium :heavy :assault
          :drive-train :standard}    ;; :standard :charger :dasher
   :speed 0.0            ;; tiles per second
   :target-speed 0.0     ;; how fast do you want to go
   :heading nil          ;; radians with 0 = east, pi/2 = south
   :target-heading 0.0   ;; where do you want to be facing (nil = no turning)

   :hp 100
   :max-hp 100

   :attack-range 1.5    ;; melee range in tiles
   :attack-damage 10    ;; per hit
   :attack-cooldown 0   ;; counts down
   :attack-speed 0.5    ;; seconds between attacks

   :hit-timer 0
   :death-timer 0

   :faction faction      ;; :player :ally :enemy
   :state :idle          ;; :idle :moving :attacking
   :path []              ;; waypoints from pathfinding
   ,})

(defn make-player [pos]
  (make-peep pos :player))

(defn make-enemy [pos]
  (make-peep pos :enemy))

(defn make-mech
  "this marks a peep as a mech unit"
  [peep chassis drive-train]
  (let [stats (chassis-size chassis)]
    (merge peep stats
      {:mech {:chassis chassis :drive-train drive-train}
       :size (:size stats)})))
