(ns inkstain.systems.tick
  (:require [inkstain.state :as state]
            [inkstain.systems.grid :as grid]
            [inkstain.input :as input])
  (:import [com.studiohartman.jamepad ControllerState]))




(defn player-input [dt]
  (let [dt-s (/ dt 1e9)
        ;; panning via held keys - applied in pixel-offset space
        held @state/*keys-held
        ;; pixels per second
        speed (-> @state/*state :player :speed)
        move-speed (* speed dt-s)]
    ;;(println :dt dt :dt-ms dt-ms :dt-s dt-s :pan-speed-dt pan-speed-dt)
    (when (seq held)
      (swap! state/*state update :player
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

    ;; bounds checking
    (swap! state/*state
      (fn [state]
        (let [grid (:grid state)
              clamp-fn (partial grid/clamp grid)]
          (-> state
            (update-in [:player :pos] clamp-fn)
            (assoc :allies (mapv (fn [ally] (update ally :pos clamp-fn)) (:allies state)))
            (assoc :enemies (mapv (fn [enemy] (update enemy :pos clamp-fn)) (:enemies state)))))))

    ;; Poll each frame in on-paint
    (let [^ControllerState state (input/get-state 0)]
      (when (input/connected? state)
        (let [lx (input/left-stick-x state)      ;; -1.0 to 1.0
              ly (input/left-stick-y state)      ;; -1.0 to 1.0
              ;; for camera later
              rx (input/right-stick-x state)
              ry (input/right-stick-y state)]
          (swap! state/*state update :player
            (fn [player]
              (let [[px py] (:pos player)
                    dx (* lx move-speed)
                    dy (* ly move-speed)]
                (assoc player :pos [(+ px dx) (+ py dy)]))))
          (cond
            (input/dpad-up state)    (swap! state/*state assoc :tactical-mode :aggressive)
            (input/dpad-down state)  (swap! state/*state assoc :tactical-mode :defensive)
            (input/dpad-left state)  (swap! state/*state assoc :tactical-mode :flank)
            (input/dpad-right state) (swap! state/*state assoc :tactical-mode :hold)))))))
