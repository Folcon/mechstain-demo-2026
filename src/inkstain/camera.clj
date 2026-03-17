(ns inkstain.camera
  (:require [inkstain.config :as config]
            [inkstain.utils :refer [clamp]]))



(defn create-camera []
  {:offset-x 0.0
   :offset-y 0.0
   :zoom 1.0
   :active-layer 1

   :pan-speed 5.0
   :pan-speed-dt 360.0
   :zoom-speed 0.1
   :min-zoom 0.5
   :max-zoom 2.5})

(defn world->screen
  ([x y] (world->screen x y 1.0 0 0))
  ([x y zoom] (world->screen x y zoom 0 0))
  ([x y zoom offset-x offset-y]
   (let [tile-size (* config/default-tile-size zoom)]
     [(+ (* x tile-size) offset-x)
      (+ (* y tile-size) offset-y)])))

(defn screen->world
  ([sx sy] (screen->world sx sy 1.0 0 0))
  ([sx sy zoom] (screen->world sx sy zoom 0 0))
  ([sx sy zoom offset-x offset-y]
   (let [tile-size (* config/default-tile-size zoom)
         wx (/ (- sx offset-x) tile-size)
         wy (/ (- sy offset-y) tile-size)]
     [(int (Math/floor wx))
      (int (Math/floor wy))])))

(defn screen->>world
  ([sx sy] (screen->world sx sy 1.0 0 0))
  ([sx sy zoom] (screen->world sx sy zoom 0 0))
  ([sx sy zoom offset-x offset-y]
   (let [tile-size (* config/default-tile-size zoom)
         wx (/ (- sx offset-x) tile-size)
         wy (/ (- sy offset-y) tile-size)]
     [wx wy])))

(defn apply-zoom [{:keys [offset-x offset-y] :as camera-state}
                  zoom mouse-x mouse-y new-zoom]
  (let [;; Calculate world position under mouse before zoom
        tile-size (* config/default-tile-size zoom)
        world-x (/ (- mouse-x offset-x) tile-size)
        world-y (/ (- mouse-y offset-y) tile-size)
        ;; Apply zoom
        new-tile-size (* config/default-tile-size new-zoom)
        ;; Calculate new offset so mouse stays over same world position
        new-offset-x (- mouse-x (* world-x new-tile-size))
        new-offset-y (- mouse-y (* world-y new-tile-size))]
    (assoc camera-state
      :offset-x new-offset-x
      :offset-y new-offset-y
      :zoom new-zoom)))

(defn visible-tiles
  "Calculate which tiles are visible on screen"
  [camera-state screen-width screen-height scale]
  (let [{:keys [zoom offset-x offset-y]} camera-state
        tile-size (* config/default-tile-size zoom scale)
        ;; Add margin for partially visible tiles
        margin 2
        min-x (- (int (Math/floor (/ (- offset-x) tile-size))) margin)
        min-y (- (int (Math/floor (/ (- offset-y) tile-size))) margin)
        max-x (+ (int (Math/ceil (/ (- screen-width offset-x) tile-size))) margin)
        max-y (+ (int (Math/ceil (/ (- screen-height offset-y) tile-size))) margin)]
    {:min-x min-x :min-y min-y :max-x max-x :max-y max-y}))
