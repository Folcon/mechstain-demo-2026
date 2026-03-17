(ns inkstain.systems.tick
  (:require [inkstain.peep :as peep]
            [inkstain.state :as state]
            [inkstain.systems.grid :as grid]
            [inkstain.input :as input]
            [inkstain.systems.pathfinding :as pathfinding]
            [inkstain.systems.movement :as movement]
            [inkstain.systems.combat :as combat])
  (:import [com.studiohartman.jamepad ControllerState]))




(defn tick-player-input [*state held dt]
  (let [;; pixels per second
        speed (-> @*state :player :speed)
        move-speed (* speed dt)

        ;; keyboard
        ;; panning via held keys - applied in pixel-offset space
        dx (cond
             (or (held :a) (held :left)) (- move-speed)
             (or (held :d) (held :right)) move-speed
             :else 0)
        dy (cond
             (or (held :w) (held :up)) (- move-speed)
             (or (held :s) (held :down)) move-speed
             :else 0)]
    (when (seq held)
      (swap! *state update :player
        (fn [player]
          (let [[px py] (:pos player)]
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

(defn tick-ally-movement [dt]
  (swap! state/*state
    (fn [state]
      (let [allies (:allies state)
            [px py] (:pos (:player state))
            grid (:grid state)]
        (assoc state :allies
          (mapv
            (fn [ally]
              (if (not (combat/alive? ally))
                ally  ;; dead/dying - don't touch
                (let [[ax ay] (:pos ally)
                      player-dist (combat/distance [px py] [ax ay])
                      [tx ty] (or (:last-target ally) [px py])
                      player-moved-dist (combat/distance [px py] [tx ty])
                      timer (- (or (:repath-timer ally) 0) dt)

                      nearest-enemy (combat/find-nearest-hostile [ax ay]
                                      (filter combat/alive? (:enemies state)))
                      enemy-dist (when nearest-enemy
                                   (combat/distance [ax ay] (:pos nearest-enemy)))
                      melee-range? (and enemy-dist (<= enemy-dist (:attack-range ally 1.5)))

                      ;; detection range
                      enemy-nearby? (and enemy-dist (<= enemy-dist 8.0))

                      ally (case (-> state :tactical-mode)
                             :aggressive
                             (cond
                               melee-range?
                               (assoc ally :state :idle :path [])  ;; stop and fight

                               ;; enemy nearby - chase it
                               (and enemy-nearby? (<= timer 0))
                               (let [[ex ey] (:pos nearest-enemy)
                                     path (pathfinding/try-search grid
                                            [(Math/round ^double ax) (Math/round ^double ay)]
                                            [(Math/round ^double ex) (Math/round ^double ey)])]
                                 (assoc ally
                                   :path (vec (or path []))
                                   :state (if path :moving :idle)
                                   :repath-timer 0.5))

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

                               :else
                               (assoc ally :repath-timer (max 0 timer)))

                             :defensive
                             (let [ally-to-player (combat/distance [ax ay] [px py])
                                   enemy-near-player? (and nearest-enemy
                                                        (<= (combat/distance [px py] (:pos nearest-enemy))
                                                          3.0))]
                               (cond
                                 ;; in melee range - fight
                                 melee-range? (assoc ally :state :idle :path [])
                                 ;; enemy near player - intercept
                                 (and enemy-near-player? (<= timer 0))
                                 (let [[ex ey] (:pos nearest-enemy)
                                       path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [(Math/round ^double ex) (Math/round ^double ey)])]
                                   (assoc ally
                                     :path (vec (or path []))
                                     :state (if path :moving :idle)
                                     :repath-timer 0.5
                                     :last-target [ex ey]))
                                 ;; too far from player - return
                                 (and (> ally-to-player 3.0) (<= timer 0))
                                 ;; pathfind back to player
                                 (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                       ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                       path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [tx ty])]
                                   (assoc ally
                                     :path (vec (or path []))
                                     :state (if path :moving :idle)
                                     :repath-timer 0.75
                                     :last-target [px py]))
                                 :else (assoc ally :repath-timer (max 0 timer))))

                             :hold
                             (if melee-range?
                               (assoc ally :state :idle :path [])
                               (assoc ally :state :idle :path [] :repath-timer (max 0 timer)))

                             :flank
                             (let [[ex ey] (:pos nearest-enemy)
                                   ;; has the ally reached the flank side?
                                   flanking? (when nearest-enemy
                                               (let [;; dot product, is ally on opposite side of enemy from player?
                                                     dx-pe (- ex px) dy-pe (- ey py)    ;; player→enemy
                                                     dx-ae (- ex ax) dy-ae (- ey ay)]   ;; ally→enemy
                                                 ;; if dot < 0, ally is on far side of enemy from player
                                                 (neg? (+ (* dx-pe dx-ae) (* dy-pe dy-ae)))))]
                               (cond
                                 melee-range? (assoc ally :state :idle :path [])

                                 ;; already flanking - close in on the enemy directly
                                 (and flanking? enemy-nearby? (<= timer 0))
                                 (let [path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [(Math/round ^double ex) (Math/round ^double ey)])]
                                   (assoc ally :path (vec (or path [])) :state (if path :moving :idle)
                                     :repath-timer 0.5))

                                 ;; not flanking yet - move to flank position
                                 (and enemy-nearby? (<= timer 0))
                                 (let [;; point on opposite side of enemy from player
                                       dx (- ex px) dy (- ey py)
                                       dist (Math/sqrt (+ (* dx dx) (* dy dy)))
                                       ;; normalise and extend 2 tiles past enemy
                                       flank-x (+ ex (* (/ dx (max dist 0.1)) 2.0))
                                       flank-y (+ ey (* (/ dy (max dist 0.1)) 2.0))
                                       ;; clamp to grid
                                       fx (max 0 (min (dec (:width grid)) (Math/round ^double flank-x)))
                                       fy (max 0 (min (dec (:height grid)) (Math/round ^double flank-y)))
                                       path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [fx fy])]
                                   (assoc ally :path (vec (or path [])) :state (if path :moving :idle)
                                     :repath-timer 0.75))
                                 (<= timer 0)
                                 ;; no enemy — follow player
                                 (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                       ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                       path (pathfinding/try-search grid [(Math/round ^double ax) (Math/round ^double ay)] [tx ty])]
                                   (assoc ally
                                     :path (vec (or path []))
                                     :state (if path :moving :idle)
                                     :repath-timer 0.75
                                     :last-target [px py]))
                                 :else (assoc ally :repath-timer (max 0 timer)))))]
                  (if (combat/alive? ally)
                    (movement/step-movement ally dt)
                    ally))))
            allies))))))

(defn tick-enemy-movement [dt]
  (swap! state/*state
    (fn [state]
      (let [[px py] (:pos (:player state))
            grid (:grid state)]
        (assoc state :enemies
          (mapv (fn [enemy]
                  (if (not (combat/alive? enemy))
                    enemy  ;; dead/dying - don't touch
                    (let [[ex ey] (:pos enemy)
                          timer (- (or (:repath-timer enemy) 0) dt)
                          enemy (if (<= timer 0)
                                  (let [path (pathfinding/try-search grid
                                               [(Math/round ^double ex) (Math/round ^double ey)]
                                               [(Math/round ^double px) (Math/round ^double py)])]
                                    (assoc enemy
                                      :path (vec (or path []))
                                      :state (if path :moving :idle)
                                      :repath-timer 1.5))
                                  (assoc enemy :repath-timer timer))]
                      (if (combat/alive? enemy)
                        (movement/step-movement enemy dt)
                        enemy))))
            (:enemies state)))))))

(defn tick-combat [dt]
  (let [{:keys [player allies enemies]}
        (combat/process-combat (:player @state/*state) (:allies @state/*state) (:enemies @state/*state) dt)]
    (swap! state/*state assoc :player player :allies allies :enemies enemies)))

(defn tick-spawning [dt]
  (when (and (> (:hp (:player @state/*state)) 0)
          (< (count (:enemies @state/*state)) 100))
    (let [timer (- (:spawn-timer @state/*state) dt)]
      (if (<= timer 0)
        (let [pos (grid/random-edge-pos (:grid @state/*state))]
          (swap! state/*state assoc :spawn-timer (:spawn-interval @state/*state))
          (swap! state/*state update :enemies conj (peep/make-enemy pos)))
        (swap! state/*state assoc :spawn-timer timer)))))
