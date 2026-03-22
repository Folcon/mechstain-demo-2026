(ns inkstain.config
  (:require [environ.core :refer [env]]
            [inkstain.version :as version]))



(def engine-name "Inkstain Engine")
(def init-window-size [1280 720])
(def version-string (version/version-string engine-name))

(def debug? (boolean (env :debug)))

(def log-effects? debug?)

(def default-tile-size 32)



(def sensor-range 12.0)  ;; tiles

(def squad-slot-config
  {:follow-radius      2.5   ;; ring distance from player when following
   :defensive-radius   2.0   ;; tighter perimeter in defensive mode
   :surround-radius    1.5   ;; how close to cluster around a target enemy
   :hold-spread        (/ Math/PI 2)  ;; 90 degree arc for hold formation
   :hold-radius        2.0
   :reassign-threshold 2.0}) ;; anchor movement before slots regenerate
