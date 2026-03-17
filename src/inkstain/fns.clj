(ns inkstain.fns
  (:require [inkstain.config :as config]
            [inkstain.state :as state]))



(defn maybe-quit [_] (if config/debug? #(reset! state/*window (promise)) #(System/exit 0)))
