(ns inkstain.peep
  (:require [inkstain.state :as state]))



(defn make-peep [pos faction]
  {:id (state/next-id)
   :pos pos              ;; grid position [x y]
   :max-speed 4.0        ;; tiles per second as a peep

   ;; mech speed etc comes from drive-train
   :mech {:chassis :medium           ;; :light :medium :heavy :assault
          :drive-train :standard}    ;; :standard :charger :dash
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
