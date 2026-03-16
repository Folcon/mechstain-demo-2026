(ns inkstain.peep
  (:require [inkstain.state :as state]))



(defn make-peep [pos faction]
  {:id (state/next-id)
   :pos pos              ;; grid position [x y]
   :speed 4.0            ;; tiles per second
   :hp 100
   :max-hp 100
   :faction faction      ;; :player :ally :enemy
   :state :idle          ;; :idle :moving :attacking
   :path []              ;; waypoints from pathfinding
   ,})

(defn make-player [pos]
  (make-peep pos :player))

(defn make-enemy [pos]
  (make-peep pos :enemy))
