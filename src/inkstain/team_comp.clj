(ns inkstain.team-comp
  (:require [io.github.humbleui.ui :as ui]
            [inkstain.input :as input]
            [inkstain.persistence :as persistence]
            [inkstain.stats :as stats]
            [inkstain.render :as render])
  (:import [clojure.lang PersistentVector]))



(defonce *team-comp
  (ui/signal nil))

(defn init! []
  (let [data (persistence/load-data)
        preset-name (:last-used data "default")
        comp (get (:presets data) preset-name persistence/default-comp)]
    (reset! *team-comp
      {:cursor [0 0]
       :comp comp
       :preset-name preset-name
       :preset-names (vec (sort (keys (:presets data))))})))

(def chassis-options [:light :medium :heavy :assault])
(def drive-train-options [:standard :charger :dasher])
(def row-cols [1 1 3 3 3 3 3 1 3])


(defn cycle-option [current options dir]
  (let [n (count options)
        idx (mod (+ (.indexOf ^PersistentVector options current) dir n) n)]
    (nth options idx)))

(defn cycle-at-cursor! [dir]
  (let [{:keys [cursor]} @*team-comp
        [row col] cursor]
    (cond
      (= row 0)
      (swap! *team-comp update-in [:comp :player :chassis]
        cycle-option chassis-options dir)

      (= row 1)
      (swap! *team-comp update-in [:comp :player :drive-train]
        cycle-option drive-train-options dir)

      (<= 2 row 6)
      (let [i (- row 2)]
        (case col
          0 (swap! *team-comp update-in [:comp :allies i :chassis]
              cycle-option chassis-options dir)
          1 (swap! *team-comp update-in [:comp :allies i :drive-train]
              cycle-option drive-train-options dir)
          2 (swap! *team-comp update-in [:comp :allies i :enabled?] not)))

      (= row 7)
      (let [{:keys [preset-name preset-names]} @*team-comp
            new-name (cycle-option preset-name preset-names dir)
            data (persistence/load-data)
            new-comp (get (:presets data) new-name persistence/default-comp)]
        (swap! *team-comp assoc
          :preset-name new-name
          :comp new-comp)))))

(defn highlight-and-cycle! [cursor]
  (swap! *team-comp assoc :cursor cursor)
  (cycle-at-cursor! 1))

(defn navigate! [direction]
  (swap! *team-comp
    (fn [{:keys [cursor] :as state}]
      (let [[row col] cursor
            [new-row new-col]
            (case direction
              :up    [(max 0 (dec row)) col]
              :down  [(min (dec (count row-cols)) (inc row)) col]
              :left  (if (= 1 (nth row-cols row))
                       [row col]  ;; handled below
                       [row (max 0 (dec col))])
              :right (if (= 1 (nth row-cols row))
                       [row col]  ;; handled below
                       [row (min (dec (nth row-cols row)) (inc col))]))
            ;; clamp col after row change
            new-col (min new-col (dec (nth row-cols new-row)))]
        (assoc state :cursor [new-row new-col]))))
  ;; for single-col rows, left/right cycles value
  (let [[row _] (:cursor @*team-comp)]
    (when (= 1 (nth row-cols row))
      (case direction
        :left  (cycle-at-cursor! -1)
        :right (cycle-at-cursor! 1)
        nil))))

(defn activate-deploy! []
  (let [{:keys [comp preset-name]} @*team-comp]
    (persistence/save-preset! preset-name comp)
    (render/init-state comp)
    (input/swap-focus! :playing)))

(defn activate-save! []
  (let [{:keys [comp preset-name]} @*team-comp]
    (persistence/save-preset! preset-name comp)
    (swap! *team-comp assoc :preset-names
      (vec (sort (keys (:presets (persistence/load-data))))))))

(defn activate! []
  (let [{:keys [cursor]} @*team-comp
        [row col] cursor]
    (cond
      (<= 0 row 7) (cycle-at-cursor! 1)
      (= row 8) (case col
                  0 (activate-save!)
                  1 (activate-deploy!)
                  2 (input/pop-focus!)))))

(defn selector [cr cc row col value on-click]
  [ui/clickable {:on-click (fn [_] (on-click))}
   [ui/rect {:paint {:fill (if (and (= cr row) (= cc col)) 0xFF446688
                             0xFF333333)}
             :radius 4}
    [ui/padding {:horizontal 12 :vertical 6}
     [ui/label {:paint {:fill 0xFFFFFFFF}}
      (if (keyword? value) (name value) (str value))]]]])

(defn toggle [cr cc row col enabled? on-click]
  [ui/clickable {:on-click (fn [_] (on-click))}
   [ui/rect {:paint {:fill (if (and (= cr row) (= cc col)) 0xFF446688
                             0xFF333333)}
             :radius 4}
    [ui/padding {:horizontal 12 :vertical 6}
     [ui/label {:paint {:fill (if enabled? 0xFF88FF88 0xFF666666)}}
      (if enabled? "ON" "OFF")]]]])

(defn button-cell [cr cc row col label on-click]
  [ui/clickable {:on-click (fn [_] (on-click))}
   [ui/rect {:paint {:fill (if (and (= cr row) (= cc col)) 0xFF446688
                             0xFF555555)}
             :radius 6}
    [ui/padding {:horizontal 16 :vertical 8}
     [ui/label {:paint {:fill 0xFFFFFFFF}} label]]]])

(ui/defcomp ui []
  (let [{:keys [cursor comp preset-name]} @*team-comp
        {:keys [player allies]} comp
        [cr cc] cursor
        stats (stats/all-combos [(:chassis player) (:drive-train player)])]
    (if-not comp
      [ui/center [ui/label "Loading..."]]
      [ui/key-listener
       {:on-key-down (fn [e]
                       (case (:key e)
                         :up    (navigate! :up)
                         :down  (navigate! :down)
                         :left  (navigate! :left)
                         :right (navigate! :right)
                         :w     (navigate! :up)
                         :s     (navigate! :down)
                         :a     (navigate! :left)
                         :d     (navigate! :right)
                         :enter (activate!)
                         :space (activate!)
                         :escape (input/pop-focus!)
                         nil))}
       [ui/rect {:paint {:fill 0xFF1A1A2E}}
        [ui/center
         (into
           [ui/column {:gap 8}
            [ui/padding {:bottom 16}
             [ui/center
               [ui/label {:font-weight :bold :paint {:fill 0xFFFFFFFF}} "TEAM COMPOSITION"]]]
            [ui/rect {:paint {:fill 0xFF222233} :radius 6}
             [ui/padding {:padding 12}
              [ui/column {:gap 6}
               [ui/label {:font-weight :bold :paint {:fill 0xFFCCCCCC}} "YOUR MECH"]
               [ui/row {:gap 12}
                [ui/size {:width 80}
                 [ui/label {:paint {:fill 0xFFAAAAAA}} "Chassis:"]]
                (selector cr cc 0 0 (:chassis player) #(highlight-and-cycle! [0 0]))]
               [ui/row {:gap 12}
                [ui/size {:width 80}
                 [ui/label {:paint {:fill 0xFFAAAAAA}} "Drive:"]]
                (selector cr cc 1 0 (:drive-train player) #(highlight-and-cycle! [1 0]))]
               [ui/label {:paint {:fill 0xFF888888}} "HP"]
               (stats/stat-pips (stats/val->pips (:hp stats) (stats/stat-ranges :hp)))
               [ui/gap]
               [ui/label {:paint {:fill 0xFF888888}} "ATK"]
               (stats/stat-pips (stats/val->pips (:atk stats) (stats/stat-ranges :atk)))
               [ui/gap]
               [ui/label {:paint {:fill 0xFF888888}} "SPD"]
               (stats/stat-pips (stats/val->pips (:spd stats) (stats/stat-ranges :spd)))
               [ui/gap]
               [ui/label {:paint {:fill 0xFF888888}} "ACC"]
               (stats/stat-pips (stats/val->pips (:acc stats) (stats/stat-ranges :acc)))]]]
            [ui/rect {:paint {:fill 0xFF223322} :radius 6}
             [ui/padding {:padding 12}
              [ui/column {:gap 6}
               [ui/label {:font-weight :bold :paint {:fill 0xFFCCCCCC}} "SQUAD"]]]]]
           cat
           [(map-indexed
              (fn [i ally]
                (let [row (+ 2 i)
                      stats (stats/all-combos [(:chassis ally) (:drive-train ally)])]
                  [ui/column
                   [ui/row {:gap 12}
                    [ui/size {:width 24}
                     [ui/label {:paint {:fill 0xFFAAAAAA}} (str (inc i) ".")]]
                    [ui/size {:width 100}
                     [selector cr cc row 0 (:chassis ally) #(highlight-and-cycle! [row 0])]]
                    [ui/size {:width 100}
                     [selector cr cc row 1 (:drive-train ally) #(highlight-and-cycle! [row 1])]]
                    [ui/label {:paint {:fill 0xFF888888}} "HP"]
                    (stats/stat-pips (stats/val->pips (:hp stats) (stats/stat-ranges :hp)))
                    [ui/gap]
                    [ui/label {:paint {:fill 0xFF888888}} "ATK"]
                    (stats/stat-pips (stats/val->pips (:atk stats) (stats/stat-ranges :atk)))
                    [ui/gap]
                    [ui/label {:paint {:fill 0xFF888888}} "SPD"]
                    (stats/stat-pips (stats/val->pips (:spd stats) (stats/stat-ranges :spd)))
                    [ui/gap]
                    [ui/label {:paint {:fill 0xFF888888}} "ACC"]
                    (stats/stat-pips (stats/val->pips (:acc stats) (stats/stat-ranges :acc)))
                    [ui/size {:width 50}
                     [toggle cr cc row 2 (:enabled? ally true) #(highlight-and-cycle! [row 2])]]]]))
              allies)
            [[ui/padding {:top 12}
              [ui/row {:gap 12}
               [ui/label {:paint {:fill 0xFFAAAAAA}} "Preset:"]
               (selector cr cc 7 0 preset-name #(cycle-at-cursor! 1))]]
             [ui/padding {:top 16}
              [ui/row {:gap 12}
               (button-cell cr cc 8 0 "Save" activate-save!)
               (button-cell cr cc 8 1 "Deploy" activate-deploy!)
               (button-cell cr cc 8 2 "Back" input/pop-focus!)]]
             [ui/padding {:top 12}
              [ui/row {:gap 12}
               [ui/label {:paint {:fill 0xFFAAAAAA}}
                "On Controller: Move with dpad + A Confirm / B Cancel"]]]
             [ui/padding {:top 12}
              [ui/row {:gap 12}
               [ui/label {:paint {:fill 0xFFAAAAAA}}
                "On Keyboard: Move with WASD + Arrow keys + Space confirm / Esc Cancel"]]]]])]]])))


(input/register-handlers! :team-comp
  {:dpad-up    (fn [_] (navigate! :up))
   :dpad-down  (fn [_] (navigate! :down))
   :dpad-left  (fn [_] (navigate! :left))
   :dpad-right (fn [_] (navigate! :right))
   :a-button   (fn [_] (activate!))
   :b-button   (fn [_] (input/pop-focus!))
   :start      (fn [_] (activate-deploy!))

   :left-stick (fn [axes]
                 (when-let [dir (input/stick->direction axes)]
                   (navigate! dir)))})
