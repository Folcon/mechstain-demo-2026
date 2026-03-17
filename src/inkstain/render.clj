(ns inkstain.render
  (:require
   [clojure.string :as str]
   [clojure.math :as math]
   [io.github.humbleui.canvas :as canvas]
   [io.github.humbleui.ui :as ui]
   [io.github.humbleui.util :as util]
   [io.github.humbleui.window :as window]
   [inkstain.config :as config]
   [inkstain.state :as state]
   [inkstain.systems.grid :as grid]
   [inkstain.camera :as camera]
   [inkstain.input :as input]
   [inkstain.systems.pathfinding :as pathfinding]
   [inkstain.systems.movement :as movement]
   [inkstain.peep :as peep]
   [inkstain.utils :as utils])
  (:import
   [com.studiohartman.jamepad ControllerState]
   [io.github.humbleui.skija Color4f Paint]
   [java.util Arrays]))



(def tick-ms
  17)

(def *state
  (ui/signal
    {:last-render (System/nanoTime)
     :camera      (camera/create-camera)
     :grid        (grid/scatter-water (grid/make-grid 300 200) 400)
     :player      (peep/make-player [5 5])
     :allies      (vec (for [_i (range 5)]
                         (peep/make-peep [(+ 4 (rand-int 3)) (+ 4 (rand-int 3))] :ally)))

     :enemies []
     :spawn-timer 0.0
     :spawn-interval 3.0
     ,}))

(defn lch->rgb ^Color4f [c]
  (-> c
    (#'ui/paint-color)
    (#'ui/color->srgb :oklch)))

(def ^Paint paint
  (ui/paint {:fill "0F0"}))

(defn random-edge-pos [grid]
  (let [{:keys [width height]} grid
        edge (rand-int 4)]
    (case edge
      0 [(rand-int width) 0]            ;; top
      1 [(rand-int width) (dec height)] ;; bottom
      2 [0 (rand-int height)]           ;; left
      3 [(dec width) (rand-int height)] ;; right
      ,)))

(defn tick [[x y]])

(defn on-paint [ctx canvas size]
  (let [;; timing
        now-ns (System/nanoTime)
        last-ns (:last-render @*state)
        dt (- now-ns last-ns)
        dt-ms (/ dt 1e6)
        dt-s (/ dt 1e9)

        {:keys [pan-speed-dt]} (:camera @*state)]
    (swap! *state assoc :last-render now-ns)

    ;; input
    (let [;; panning via held keys - applied in pixel-offset space
          held @state/*keys-held
          ;; pixels per second
          speed (-> @*state :player :speed)
          move-speed (* speed dt-s)]
      ;;(println :dt dt :dt-ms dt-ms :dt-s dt-s :pan-speed-dt pan-speed-dt)
      (when (seq held)
        (swap! *state update :player
          (fn [player]
            (let [[px py] (:pos player)
                  dx (cond
                       (or (held :a) (held :left)) (- move-speed)
                       (or (held :d) (held :right)) move-speed
                       :else 0)
                  dy (cond
                       (or (held :w) (held :up)) (- move-speed)
                       (or (held :s) (held :down)) move-speed
                       :else 0)]
              (assoc player :pos [(+ px dx) (+ py dy)])))))

      ;; Poll each frame in on-paint
      (let [^ControllerState state (input/get-state 0)]
        (when (input/connected? state)
          (let [lx (input/left-stick-x state)      ;; -1.0 to 1.0
                ly (input/left-stick-y state)      ;; -1.0 to 1.0
                rx (input/right-stick-x state)     ;; for camera later
                ry (input/right-stick-y state)]
            (swap! *state update :player
              (fn [player]
                (let [[px py] (:pos player)
                      dx (* lx move-speed)
                      dy (* ly move-speed)]
                  (assoc player :pos [(+ px dx) (+ py dy)]))))))))

    (swap! *state
      (fn [state]
        (let [allies (:allies state)
              [px py] (:pos (:player state))
              grid (:grid state)]
          (assoc state :allies
            (mapv
              (fn [ally]
                (let [[ax ay] (:pos ally)
                      player-dist (Math/sqrt (+ (Math/pow (- px ax) 2) (Math/pow (- py ay) 2)))
                      [tx ty] (or (:last-target ally) [px py])
                      player-moved-dist (Math/sqrt (+ (Math/pow (- px tx) 2) (Math/pow (- py ty) 2)))
                      timer (- (or (:repath-timer ally) 0) dt-s)
                      ally (if
                             (or
                               ;; repath toward player with offset
                               (and (> player-dist 3.0) (<= timer 0))
                               ;; repath towards player who's moved
                               (and (= (:state ally) :idle) (> player-moved-dist 2.0)))
                             (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                   ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                   path (pathfinding/try-search grid [(Math/round ^double ax) (Math/round ^double ay)] [tx ty])]
                               (assoc ally
                                 :path (vec (or path []))
                                 :state (if path :moving :idle)
                                 :repath-timer 0.75
                                 :last-target [px py]))
                             (assoc ally :repath-timer (max 0 timer)))]
                  (movement/step-movement ally dt-s)))
              allies)))))

    (swap! *state
      (fn [state]
        (let [[px py] (:pos (:player state))
              grid (:grid state)]
          (assoc state :enemies
            (mapv (fn [enemy]
                    (let [[ex ey] (:pos enemy)
                          timer (- (or (:repath-timer enemy) 0) dt-s)
                          enemy (if (<= timer 0)
                                  (let [path (pathfinding/try-search grid
                                               [(Math/round ^double ex) (Math/round ^double ey)]
                                               [(Math/round ^double px) (Math/round ^double py)])]
                                    (assoc enemy
                                      :path (vec (or path []))
                                      :state (if path :moving :idle)
                                      :repath-timer 1.5))
                                  (assoc enemy :repath-timer timer))]
                      (movement/step-movement enemy dt-s)))
                (:enemies state))))))

    ;; smoothly zoom
    (let [{:keys [zoom target-zoom zoom-mouse]} (:camera @*state)]
      (when (and zoom-mouse (not= zoom target-zoom))
        (let [lerp-speed 0.15  ;; 15% of remaining distance per frame
              new-zoom (+ zoom (* (- target-zoom zoom) lerp-speed))
              ;; snap if close enough
              new-zoom (if (< (abs (- new-zoom target-zoom)) 0.001) target-zoom
                         new-zoom)]
          (swap! *state assoc-in [:camera :zoom] new-zoom))))

    ;; Center camera on player
    (let [[px py] (:pos (:player @*state))
          scale (:scale ctx)
          {screen-w :width screen-h :height} size
          tile-size-px (* config/default-tile-size (:zoom (:camera @*state))
                         scale)
          ;; offset so player tile center is at screen center
          target-ox (- (/ screen-w 2) (* (+ px 0.5) tile-size-px))
          target-oy (- (/ screen-h 2) (* (+ py 0.5) tile-size-px))]
      (swap! *state update :camera assoc :offset-x target-ox :offset-y target-oy))

    (let [{:keys [offset-x offset-y zoom]} (:camera @*state)
          scale (:scale ctx)
          tile-size    (* config/default-tile-size zoom)
          tile-size-px (* tile-size scale) ;; actual pixels (retina)
          {screen-w :width screen-h :height} size
          grid         (:grid @*state)]

      ;; tick
      (when (> dt-ms tick-ms)
        (tick (some->> @*state :producing (mapv #(quot % tile-size-px)))))

      ;; spawn timer
      (let [timer (- (:spawn-timer @*state) dt-s)]
        (if (<= timer 0)
          (let [pos (random-edge-pos (:grid @*state))]
            (swap! *state assoc :spawn-timer (:spawn-interval @*state))
            (swap! *state update :enemies conj (peep/make-enemy pos)))
          (swap! *state assoc :spawn-timer timer)))

      ;; render
      (canvas/clear canvas 0xFF1A1A2E)
      ;; draw grid - translate by offset, scale so 1 unit = 1 tile
      (canvas/with-canvas canvas
        (canvas/translate canvas offset-x offset-y)
        (canvas/scale canvas tile-size-px tile-size-px)

        ;; calculate visible tile range from camera state
        (let [{:keys [min-x min-y max-x max-y]}
              (camera/visible-tiles (:camera @*state) screen-w screen-h scale)
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

          (let [player (:player @*state)
                [px py] (:pos player)]
            (.setColor4f paint (Color4f. 0.9 0.2 0.2 1.0))  ;; red
            ;; draw at tile center (px+0.5, py+0.5), radius ~0.4 tiles
            (canvas/draw-circle canvas (+ px 0.5) (+ py 0.5) 0.4 paint))

          (doseq [ally (:allies @*state)]
            (let [[ax ay] (:pos ally)
                  path (:path ally)]
              (.setColor4f paint (Color4f. 0.3 0.5 0.9 1.0))
              (canvas/draw-circle canvas (+ ax 0.5) (+ ay 0.5) 0.4 paint)

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

          ,)))

    (doseq [enemy (:enemies @*state)]
      (let [[ex ey] (:pos enemy)
            path (:path enemy)]
        (.setColor4f paint (Color4f. 0.9 0.4 0.1 1.0))  ;; orange
        (canvas/draw-circle canvas (+ ex 0.5) (+ ey 0.5) 0.4 paint)
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
              (util/point (+ x2 0.5) (+ y2 0.5)) paint)))))

    (window/request-frame (:window ctx))))

(defn on-event [ctx evt]
  (println (pr-str evt))
  (when (= :mouse-scroll (:event evt))
    (let [{:keys [zoom target-zoom zoom-speed min-zoom max-zoom]} (:camera @*state)
          scroll (Math/signum ^double (:delta-y evt))
          new-target (utils/clamp (+ (or target-zoom zoom) (* scroll zoom-speed)) min-zoom max-zoom)]
      (swap! *state assoc-in [:camera :target-zoom] new-target)
      (swap! *state assoc-in [:camera :zoom-mouse] [(:x evt) (:y evt)])))
  #_
  (when (= :key (:event evt))
    (condp contains? key
      #{:w :up}    (swap! *state update-in [:camera :offset-y] - 1)
      #{:s :down}  (swap! *state update-in [:camera :offset-y] + 1)
      #{:a :left}  (swap! *state update-in [:camera :offset-x] - 1)
      #{:d :right} (swap! *state update-in [:camera :offset-x] + 1))
    (case (:key evt)
      :w (swap! *state update-in [:camera :offset-y] - 1)
      :s (swap! *state update-in [:camera :offset-y] + 1)
      :a (swap! *state update-in [:camera :offset-x] - 1)
      :d (swap! *state update-in [:camera :offset-x] + 1)
      :up (swap! *state update-in [:camera :offset-y] - 1)
      :down (swap! *state update-in [:camera :offset-y] + 1)
      :left (swap! *state update-in [:camera :offset-x] - 1)
      :right (swap! *state update-in [:camera :offset-x] + 1)
      nil))
  ,)

(ui/defcomp ui []
  [ui/stack
   [ui/key-listener
    {:on-key-down (fn [e]
                    (swap! state/*keys-held conj (:key e)))
     :on-key-up   (fn [e]
                    (swap! state/*keys-held disj (:key e)))}
    [ui/canvas
     {:on-paint #'on-paint
      :on-event #'on-event}]]
   [ui/padding {:padding 5}
    [ui/label (str (-> @*state :camera ((juxt :offset-x :offset-y))))]]])
