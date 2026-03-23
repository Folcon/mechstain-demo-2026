(ns inkstain.game
  (:require [io.github.humbleui.ui :as ui]
            [inkstain.state :as state]
            [inkstain.fns :as fns]
            [inkstain.input :as input]
            [inkstain.team-comp :as team-comp]
            [inkstain.render :as render]))



(defn menu-button [label focused? on-click]
  [ui/clickable {:on-click (fn [_] (on-click))}
   [ui/rect {:paint {:fill (if focused? 0xFF446688 0xFF555555)} :radius 6}
    [ui/padding {:horizontal 20 :vertical 10}
     [ui/label {:paint {:fill 0xFFFFFFFF}
                :font-weight (if focused? :bold :normal)}
      label]]]])

(defn menu-nav [cursor-atom n direction]
  (case direction
    (:up :left)    (swap! cursor-atom #(mod (dec %) n))
    (:down :right) (swap! cursor-atom #(mod (inc %) n))
    nil))

(defonce *menu-cursor (ui/signal 0))

(def menu-actions
  [{:label "Start"
    :action (fn []
              (team-comp/init!)
              (input/push-focus! :team-comp))}
   {:label "Quit"
    :action #(fns/maybe-quit nil)}])

(ui/defcomp menu-screen []
  (let [cursor @*menu-cursor]
    [ui/key-listener
     {:on-key-down (fn [e]
                     (case (:key e)
                       (:up :w)    (menu-nav *menu-cursor 2 :up)
                       (:down :s)  (menu-nav *menu-cursor 2 :down)
                       (:enter :space) ((:action (nth menu-actions cursor)))
                       :escape     (fns/maybe-quit nil)
                       nil))}
     [ui/center
      (into [ui/column {:gap 20}
             [ui/center
              [ui/label {:font-weight :bold} "MECHSTAIN 2026"]]
             [ui/center
              [ui/label "7DRL (unofficial)"]]]
        (map-indexed
          (fn [idx {:keys [label action]}]
            (menu-button label (= cursor idx) action)))
        menu-actions)]]))

(input/register-handlers! :menu
  {:dpad-up    (fn [_] (menu-nav *menu-cursor 2 :up))
   :dpad-down  (fn [_] (menu-nav *menu-cursor 2 :down))
   :a-button   (fn [_] ((:action (nth menu-actions @*menu-cursor))))
   :b-button   (fn [state] (fns/maybe-quit state))
   :left-stick (fn [axes]
                 (when-let [dir (input/stick->direction axes)]
                   (menu-nav *menu-cursor 2 dir)))})


(defonce *pause-cursor (ui/signal 0))

(def pause-actions
  [{:label "Resume"
    :action #(input/swap-focus! :playing)}
   {:label "Return to Menu"
    :action #(input/clear-focus!)}])

(ui/defcomp pause-screen []
  (let [cursor @*pause-cursor]
    [ui/key-listener
     {:on-key-down (fn [e]
                     (case (:key e)
                       (:up :w)    (menu-nav *pause-cursor 2 :up)
                       (:down :s)  (menu-nav *pause-cursor 2 :down)
                       (:enter :space) ((:action (nth pause-actions cursor)))
                       :escape     ((:action (first pause-actions)))
                       nil))}
     [ui/stack
      [render/ui]  ;; game still visible behind
      [ui/rect {:paint {:fill 0x80000000}}  ;; dark overlay
       [ui/center
        (into [ui/column {:gap 20}
               [ui/center
                [ui/label {:font-weight :bold} "PAUSED"]]]
          (map-indexed
            (fn [idx {:keys [label action]}]
              (menu-button label (= cursor idx) action)))
          pause-actions)]]]]))

(input/register-handlers! :paused
  {:dpad-up    (fn [_] (menu-nav *pause-cursor 2 :up))
   :dpad-down  (fn [_] (menu-nav *pause-cursor 2 :down))
   :a-button   (fn [_] ((:action (nth pause-actions @*pause-cursor))))
   :b-button   (fn [_] (input/clear-focus!))
   :left-stick (fn [axes]
                 (when-let [dir (input/stick->direction axes)]
                   (menu-nav *pause-cursor 2 dir)))})

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
