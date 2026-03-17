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
   [inkstain.utils :as utils])
  (:import
   [io.github.humbleui.skija Color4f Paint]
   [java.util Arrays]))



(def tick-ms
  17)

(def *state
  (ui/signal
    {:last-render (System/nanoTime)
     :camera      (camera/create-camera)
     :grid        (grid/scatter-water (grid/make-grid 300 200) 20)}))

(defn lch->rgb ^Color4f [c]
  (-> c
    (#'ui/paint-color)
    (#'ui/color->srgb :oklch)))

(def ^Paint paint
  (ui/paint {:fill "0F0"}))

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
          pan-speed (* pan-speed-dt dt-s)]
      ;;(println :dt dt :dt-ms dt-ms :dt-s dt-s :pan-speed-dt pan-speed-dt)
      (when (seq held)
        (swap! *state update :camera
          (fn [cam]
            (cond-> cam
              (held :w)     (update :offset-y + pan-speed)
              (held :s)     (update :offset-y - pan-speed)
              (held :a)     (update :offset-x + pan-speed)
              (held :d)     (update :offset-x - pan-speed)
              (held :up)    (update :offset-y + pan-speed)
              (held :down)  (update :offset-y - pan-speed)
              (held :left)  (update :offset-x + pan-speed)
              (held :right) (update :offset-x - pan-speed))))))

    (let [{:keys [offset-x offset-y zoom]} (:camera @*state)
          scale (:scale ctx)
          tile-size    (* config/default-tile-size zoom)
          tile-size-px (* tile-size scale) ;; actual pixels (retina)
          {screen-w :width screen-h :height} size
          grid         (:grid @*state)]

      ;; tick
      (when (> dt-ms tick-ms)
        (tick (some->> @*state :producing (mapv #(quot % tile-size-px)))))

      ;; render
      (canvas/clear canvas 0xFF1A1A2E)
      ;; draw grid - translate by offset, scale so 1 unit = 1 tile
      (canvas/with-canvas canvas
        (canvas/translate canvas offset-x offset-y)
        (canvas/scale canvas tile-size-px tile-size-px)

        ;; calculate visible tile range from camera state
        (let [{:keys [min-x min-y max-x max-y]}
              (camera/visible-tiles (:camera @*state) screen-w screen-h)
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
          ,)))

    ;; draw entities on top

    (window/request-frame (:window ctx))))

(defn on-event [ctx evt]
  (println (pr-str evt))
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
    [ui/mouse-listener
     {:on-scroll (fn [e]
                   (let [{:keys [zoom zoom-speed min-zoom max-zoom offset-x offset-y]} (:camera @*state)
                         new-zoom (utils/clamp (+ zoom (* (:delta-y e) zoom-speed)) min-zoom max-zoom)]
                     (swap! *state update :camera camera/apply-zoom zoom (:x e) (:y e) new-zoom)))}
     [ui/canvas
      {:on-paint #'on-paint
       :on-event #'on-event}]]]
   [ui/label (str (-> @*state :camera ((juxt :offset-x :offset-y))))]])
