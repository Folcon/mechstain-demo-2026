(ns inkstain.fns
  (:require [io.github.humbleui.app :as app]
            [inkstain.config :as config]
            [inkstain.state :as state]))



(defn maybe-quit [_] (if config/debug? (reset! state/*window (promise)) (app/terminate)))
