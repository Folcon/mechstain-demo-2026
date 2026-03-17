(ns inkstain.config
  (:require [environ.core :refer [env]]
            [inkstain.version :as version]))



(def engine-name "Inkstain Engine")
(def init-window-size [1280 720])
(def version-string (version/version-string engine-name))

(def debug? (boolean (env :debug)))

(def log-effects? debug?)

(def default-tile-size 32)
