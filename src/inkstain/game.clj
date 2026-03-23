(ns inkstain.game
  (:require [io.github.humbleui.ui :as ui]
            [inkstain.state :as state]
            [inkstain.fns :as fns]
            [inkstain.input :as input]
            [inkstain.team-comp :as team-comp]
            [inkstain.render :as render]))



(ui/defcomp menu-screen []
  [ui/center
   [ui/column {:gap 20}
    [ui/center
     [ui/label {:font-weight :bold} "MECHSTAIN"]]
    [ui/button {:on-click (fn [_]
                            (team-comp/init!)
                            (input/push-focus! :team-comp))}
     "Start"]
    [ui/button {:on-click fns/maybe-quit}
     "Quit"]]])

(input/register-handlers! :menu
  {:a-button   (fn [_]
                 (team-comp/init!)
                 (input/push-focus! :team-comp))
   :b-button   (fn [state] (fns/maybe-quit state))})


(ui/defcomp pause-screen []
  [ui/stack
   [render/ui]  ;; game still visible behind
   [ui/rect {:paint {:fill 0x80000000}}  ;; dark overlay
    [ui/center
     [ui/column {:gap 20}
      [ui/center
       [ui/label {:font-weight :bold} "PAUSED"]]
      [ui/button {:on-click (fn [_] (input/swap-focus! :playing))}
       "Resume"]
      [ui/button {:on-click (fn [_] (input/clear-focus!))}
       "Return to Menu"]]]]])

(input/register-handlers! :paused
  {:a-button   (fn [_] (input/swap-focus! :playing))
   :b-button   (fn [_] (input/clear-focus!))})

(ui/defcomp death-screen []
  (let [score (:score @state/*state)]
    [ui/center
     [ui/column {:gap 20}
      [ui/center
       [ui/label {:font-weight :bold} "YOU DIED"]]
      [ui/center
       [ui/label (str "Time: "  (long (:time-alive score)) "s")]]
      [ui/center
       [ui/label (str "Kills: " (:kills score))]]
      [ui/center
       [ui/label (str "Scrap: " (:scrap score))]]
      [ui/button {:on-click (fn [_] (input/pop-focus!))}
       "Return to Menu"]]]))

(input/register-handlers! :dead
  {:a-button   (fn [_] (input/pop-focus!))})

(ui/defcomp game-root []
  [ui/key-listener
   {:on-key-down (fn [e]
                   (when (= :escape (:key e))
                     (case (peek @state/*screen)
                       :playing   (input/push-focus! :paused)
                       :paused    (input/pop-focus!)
                       :team-comp (input/pop-focus!)
                       nil)))}
   (do (println :*screen @state/*screen)
     (case (peek @state/*screen)
       :menu      [menu-screen]
       :team-comp [team-comp/ui]
       :playing   [render/ui]
       :paused    [pause-screen]
       :dead      [death-screen]))])

(input/register-handlers! :playing
  {:start   (fn [_] (input/swap-focus! :paused))})
