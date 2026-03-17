(ns inkstain.config
  (:require [environ.core :refer [env]]
            [inkstain.version :as version]))



(def engine-name "Inkstain Engine")
(def init-window-size [1280 720])
(def version-string (version/version-string engine-name))

(def log-effects? (env :debug))

(def default-tile-size 32)
