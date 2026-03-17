(ns inkstain.render
  (:require
   [io.github.humbleui.canvas :as canvas]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.util :as util]
   [io.github.humbleui.window :as window]
   [inkstain.config :as config]
   [inkstain.state :as state]
   [inkstain.systems.grid :as grid]
   [inkstain.camera :as camera]
   [inkstain.input :as input]
   [inkstain.systems.tick :as tick]
   [inkstain.peep :as peep]
   [inkstain.utils :as utils])
  (:import
   [com.studiohartman.jamepad ControllerState]
   [io.github.humbleui.skija Color4f Paint]))



(def tick-ms
  17)

(defn init-state []
  (reset! state/*state
    {:last-render (System/nanoTime)
     :camera      (camera/create-camera)
     :grid        (grid/scatter-water (grid/make-grid 300 200) 400)
     :player      (peep/make-player [5 5])
     :allies      (vec (for [_i (range 5)]
                         (peep/make-peep [(+ 4 (rand-int 3)) (+ 4 (rand-int 3))] :ally)))

     :enemies []
     :spawn-timer 0.0
     :spawn-interval 3.0

     :tactical-mode :aggressive
     ,}))

(defn lch->rgb ^Color4f [c]
  (-> c
    (#'ui/paint-color)
    (#'ui/color->srgb :oklch)))

(def ^Paint paint
  (ui/paint {:fill "0F0"}))

(defn draw-hp-bar [canvas paint x y hp max-hp]
  (let [bar-width 0.8
        bar-height 0.08
        bar-x (+ x 0.1)
        bar-y (- y 0.15)
        hp-frac (max 0 (/ (double hp) max-hp))]
    ;; background (dark)
    (.setColor4f ^Paint paint (Color4f. 0.2 0.2 0.2 0.8))
    (canvas/draw-rect canvas (util/rect-xywh bar-x bar-y bar-width bar-height) paint)
    ;; health portion (green → red)
    (.setColor4f ^Paint paint (Color4f. (float (- 1.0 hp-frac)) (float hp-frac) 0.1 0.9))
    (canvas/draw-rect canvas (util/rect-xywh bar-x bar-y (* bar-width hp-frac) bar-height) paint)))

(defn draw-hit [entity normal-colour]
  (if (pos? (:hit-timer entity 0))
    (.setColor4f paint (Color4f. 1.0 1.0 1.0 1.0))  ;; white flash
    (.setColor4f paint normal-colour)))

(defn on-paint [ctx canvas size]
  (let [;; timing
        now-ns (System/nanoTime)
        last-ns (:last-render @state/*state)
        dt (/ (- now-ns last-ns) 1e9)]
    (swap! state/*state assoc :last-render now-ns)

    ;; one tick, one swap
    (when (= :playing @state/*screen)
      (let [held @state/*keys-held
            ;; Poll each frame in on-paint
            controller (when-let [^ControllerState cs (input/get-state 0)]
                         (when (input/connected? cs)
                           cs))]
        (swap! state/*state tick/tick {:held held :controller controller :dt dt})))

    ;; smoothly zoom
    (let [{:keys [zoom target-zoom zoom-mouse]} (:camera @state/*state)]
      (when (and zoom-mouse (not= zoom target-zoom))
        (let [lerp-speed 0.15  ;; 15% of remaining distance per frame
              new-zoom (+ zoom (* (- target-zoom zoom) lerp-speed))
              ;; snap if close enough
              new-zoom (if (< (abs (- new-zoom target-zoom)) 0.001) target-zoom
                         new-zoom)]
          (swap! state/*state assoc-in [:camera :zoom] new-zoom))))

    ;; Center camera on player
    (let [[px py] (:pos (:player @state/*state))
          scale (:scale ctx)
          {screen-w :width screen-h :height} size
          tile-size-px (* config/default-tile-size (:zoom (:camera @state/*state))
                         scale)
          ;; offset so player tile center is at screen center
          target-ox (- (/ screen-w 2) (* (+ px 0.5) tile-size-px))
          target-oy (- (/ screen-h 2) (* (+ py 0.5) tile-size-px))]
      (swap! state/*state update :camera assoc :offset-x target-ox :offset-y target-oy))

    (let [{:keys [offset-x offset-y zoom]} (:camera @state/*state)
          scale (:scale ctx)
          tile-size    (* config/default-tile-size zoom)
          tile-size-px (* tile-size scale) ;; actual pixels (retina)
          {screen-w :width screen-h :height} size
          grid         (:grid @state/*state)]

      ;; render
      (canvas/clear canvas 0xFF1A1A2E)
      ;; draw grid - translate by offset, scale so 1 unit = 1 tile
      (canvas/with-canvas canvas
        (canvas/translate canvas offset-x offset-y)
        (canvas/scale canvas tile-size-px tile-size-px)

        ;; calculate visible tile range from camera state
        (let [{:keys [min-x min-y max-x max-y]}
              (camera/visible-tiles (:camera @state/*state) screen-w screen-h scale)
              min-x (max 0 min-x)
              min-y (max 0 min-y)
              max-x (min (:width grid) max-x)
              max-y (min (:height grid) max-y)]

          (doseq [y (range min-y max-y)
                  x (range min-x max-x)]
            (if (grid/walkable? grid x y)
              (.setColor4f paint (Color4f. 0.3 0.6 0.2 1.0))  ;; green
              (.setColor4f paint (Color4f. 0.2 0.3 0.8 1.0)))  ;; blue for water
            (canvas/draw-rect canvas (util/rect-xywh x y 1 1) paint))

          ;; grid lines
          (.setColor4f paint (Color4f. 0.0 0.0 0.0 0.15))
          (doseq [x (range min-x (inc max-x))]
            (canvas/draw-line canvas (util/point x min-y) (util/point x max-y) paint))
          (doseq [y (range min-y (inc max-y))]
            (canvas/draw-line canvas (util/point min-x y) (util/point max-x y) paint))

          (let [player (:player @state/*state)
                [px py] (:pos player)]
            (draw-hit player (Color4f. 0.9 0.2 0.2 1.0))  ;; red
            (let [death-timer (:death-timer player 0)
                  radius (if (= :dying (:state player))
                           (* 0.4 (/ death-timer 0.5))  ;; shrink from 0.4 to 0
                           0.4)]
              ;; draw at tile center (px+0.5, py+0.5), radius ~0.4 tiles
              (canvas/draw-circle canvas (+ px 0.5) (+ py 0.5) radius paint))
            (draw-hp-bar canvas paint px py (:hp player) (:max-hp player)))

          (doseq [ally (:allies @state/*state)]
            (let [[ax ay] (:pos ally)
                  path (:path ally)]
              (draw-hit ally (Color4f. 0.3 0.5 0.9 1.0))  ;; light blue
              (let [death-timer (:death-timer ally 0)
                    radius (if (= :dying (:state ally))
                             (* 0.4 (/ death-timer 0.5))  ;; shrink from 0.4 to 0
                             0.4)]
                (canvas/draw-circle canvas (+ ax 0.5) (+ ay 0.5) radius paint))
              (draw-hp-bar canvas paint ax ay (:hp ally) (:max-hp ally))

              ;; draw debug pathfinding path
              (when (seq path)
                (.setColor4f paint (Color4f. 1.0 1.0 0.0 0.5))  ;; yellow, semi-transparent
                ;; line from current pos to first waypoint
                (let [[fx fy] (first path)]
                  (canvas/draw-line canvas
                    (util/point (+ ax 0.5) (+ ay 0.5))
                    (util/point (+ fx 0.5) (+ fy 0.5))
                    paint))
                ;; line between waypoints
                (doseq [[[x1 y1] [x2 y2]] (partition 2 1 path)]
                  (canvas/draw-line canvas
                    (util/point (+ x1 0.5) (+ y1 0.5))
                    (util/point (+ x2 0.5) (+ y2 0.5))
                    paint)))))

          ,)

        (doseq [enemy (:enemies @state/*state)]
          (let [[ex ey] (:pos enemy)
                path (:path enemy)]
            (draw-hit enemy (Color4f. 0.9 0.4 0.1 1.0))  ;; orange
            (let [death-timer (:death-timer enemy 0)
                  radius (if (= :dying (:state enemy))
                           (* 0.4 (/ death-timer 0.5))  ;; shrink from 0.4 to 0
                           0.4)]
              (canvas/draw-circle canvas (+ ex 0.5) (+ ey 0.5) radius paint))
            (draw-hp-bar canvas paint ex ey (:hp enemy) (:max-hp enemy))
            ;; path debug
            (when (seq path)
              (.setColor4f paint (Color4f. 1.0 0.2 0.2 0.5))  ;; red, semi-transparent
              (let [[fx fy] (first path)]
                (canvas/draw-line canvas
                  (util/point (+ ex 0.5) (+ ey 0.5))
                  (util/point (+ fx 0.5) (+ fy 0.5)) paint))
              (doseq [[[x1 y1] [x2 y2]] (partition 2 1 path)]
                (canvas/draw-line canvas
                  (util/point (+ x1 0.5) (+ y1 0.5))
                  (util/point (+ x2 0.5) (+ y2 0.5)) paint)))))))

    (window/request-frame (:window ctx))))

(defn on-event [ctx evt]
  (println (pr-str evt))
  (when (= :mouse-scroll (:event evt))
    (let [{:keys [zoom target-zoom zoom-speed min-zoom max-zoom]} (:camera @state/*state)
          scroll (Math/signum ^double (:delta-y evt))
          new-target (utils/clamp (+ (or target-zoom zoom) (* scroll zoom-speed)) min-zoom max-zoom)]
      (swap! state/*state assoc-in [:camera :target-zoom] new-target)
      (swap! state/*state assoc-in [:camera :zoom-mouse] [(:x evt) (:y evt)])))
  #_
  (when (= :key (:event evt))
    (condp contains? key
      #{:w :up}    (swap! state/*state update-in [:camera :offset-y] - 1)
      #{:s :down}  (swap! state/*state update-in [:camera :offset-y] + 1)
      #{:a :left}  (swap! state/*state update-in [:camera :offset-x] - 1)
      #{:d :right} (swap! state/*state update-in [:camera :offset-x] + 1))
    (case (:key evt)
      :w (swap! state/*state update-in [:camera :offset-y] - 1)
      :s (swap! state/*state update-in [:camera :offset-y] + 1)
      :a (swap! state/*state update-in [:camera :offset-x] - 1)
      :d (swap! state/*state update-in [:camera :offset-x] + 1)
      :up (swap! state/*state update-in [:camera :offset-y] - 1)
      :down (swap! state/*state update-in [:camera :offset-y] + 1)
      :left (swap! state/*state update-in [:camera :offset-x] - 1)
      :right (swap! state/*state update-in [:camera :offset-x] + 1)
      nil))
  ,)

(init-state)

(ui/defcomp ui []
  [ui/stack
   [ui/key-listener
    {:on-key-down (fn [e]
                    (swap! state/*keys-held conj (:key e))
                    (case (:key e)
                      :digit1 (swap! state/*state assoc :tactical-mode :aggressive)
                      :digit2 (swap! state/*state assoc :tactical-mode :defensive)
                      :digit3 (swap! state/*state assoc :tactical-mode :hold)
                      :digit4 (swap! state/*state assoc :tactical-mode :flank)
                      nil))
     :on-key-up   (fn [e]
                    (swap! state/*keys-held disj (:key e)))}
    [ui/canvas
     {:on-paint #'on-paint
      :on-event #'on-event}]]
   [ui/padding {:padding 5}
    [ui/align {:x :right :y :bottom}
     [ui/label (str (-> @state/*state :camera ((juxt :offset-x :offset-y))))]]]
   [ui/padding {:padding 5}
    [ui/align {:x :right :y :top}
     [ui/label {:font-weight :bold}
      (str "Mode: " (name (:tactical-mode @state/*state)))]]]])
