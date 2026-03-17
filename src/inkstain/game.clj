(ns inkstain.game
  (:require [io.github.humbleui.ui :as ui]
            [inkstain.fns :as fns]
            [inkstain.render :as render]))



(def *screen (atom :menu))  ;; :menu :playing :paused :dead
(def *score (atom {:kills 0 :time 0}))

(ui/defcomp menu-screen []
  [ui/center
   [ui/column {:gap 20}
    [ui/center
     [ui/label {:font-weight :bold} "MECHSTAIN"]]
    [ui/button {:on-click (fn [_]
                            (reset! render/*state (render/init-state))
                            (reset! *score {:kills 0 :time 0})
                            (reset! *screen :playing))}
     "Start"]
    [ui/button {:on-click fns/maybe-quit}
     "Quit"]]])


(ui/defcomp game-root []
  [ui/key-listener
   {:on-key-down (fn [e]
                   (when (= :escape (:key e))
                     (case @*screen
                       :playing (reset! *screen :paused)
                       :paused  (reset! *screen :playing)
                       nil)))}
   (case @*screen
     :menu    [menu-screen]
     :playing [render/ui])])
