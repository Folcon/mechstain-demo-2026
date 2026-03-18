(ns inkstain.peep
  (:require [inkstain.state :as state]))



(defn make-peep [pos faction]
  {:id (state/next-id)
   :pos pos              ;; grid position [x y]

   :max-speed 4.0        ;; tiles per second
   :speed 4.0            ;; tiles per second
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
