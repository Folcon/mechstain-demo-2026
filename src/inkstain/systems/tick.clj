(ns inkstain.systems.tick
  (:require [inkstain.config :as config]
            [inkstain.peep :as peep]
            [inkstain.state :as state]
            [inkstain.systems.grid :as grid]
            [inkstain.input :as input]
            [inkstain.math :as math]
            [inkstain.systems.collision :as collision]
            [inkstain.systems.pathfinding :as pathfinding]
            [inkstain.systems.corridor :as corridor]
            [inkstain.systems.movement :as movement]
            [inkstain.systems.combat :as combat]))




(defn tick-player-input [state {:keys [held controller dt]}]
  (let [;; pixels per second
        max-speed (or (-> state :player :mech movement/effective-drive-train :max-speed)
                    (-> state :player :max-speed))

        ;; keyboard
        ;; panning via held keys - applied in pixel-offset space
        dx-kb (cond
                (or (held :a) (held :left)) -1.0
                (or (held :d) (held :right)) 1.0
                :else 0)
        dy-kb (cond
                (or (held :w) (held :up))   -1.0
                (or (held :s) (held :down))  1.0
                :else 0)

        ;; controller
        [lx ly rx ry] (if controller
                        [(input/left-stick-x controller)    ;; -1.0 to 1.0
                         (input/left-stick-y controller)    ;; -1.0 to 1.0
                         ;; for camera later
                         (input/right-stick-x controller)
                         (input/right-stick-y controller)]
                        [0 0 0 0])

        ;; combine and clamp to unit length
        dx (+ dx-kb lx)
        dy (+ dy-kb ly)
        magnitude (Math/sqrt (+ (* dx dx) (* dy dy)))

        tactical-mode (when controller
                        (cond
                          (input/dpad-up controller) :aggressive
                          (input/dpad-down controller) :defensive
                          (input/dpad-left controller) :flank
                          (input/dpad-right controller) :hold))]
    (-> state
      (update :player
        (fn [player]
          (if (> magnitude 0.01)
            ;; if input, set target heading and desired speed
            (let [target-heading (Math/atan2 dy dx)
                  ;; if analog stick, magnitude controls speed (0-1 range)
                  speed-frac (min 1.0 magnitude)]
              (assoc player
                :target-heading target-heading
                :target-speed (* max-speed speed-frac)))
            ;; if no input, decelerate to stop
            (assoc player :target-speed 0.0))))
      (cond->
        tactical-mode (assoc :tactical-mode tactical-mode)))))

(defn tick-ally-movement [state dt]
  (let [allies (:allies state)
        [px py] (:pos (:player state))
        grid (:grid state)]
    (assoc state :allies
      (mapv
        (fn [ally]
          (if (not (combat/alive? ally))
            ally  ;; dead/dying - don't touch
            (let [[ax ay] (:pos ally)
                  player-dist (math/distance [px py] [ax ay])
                  [tx ty] (or (:last-target ally) [px py])
                  player-moved-dist (math/distance [px py] [tx ty])
                  timer (- (or (:repath-timer ally) 0) dt)

                  sensor-range config/sensor-range
                  in-sensor-range? (fn [e] (< (math/distance [ax ay] (:pos e)) sensor-range))
                  nearest-enemy (combat/find-nearest-hostile [px py]
                                  (filter (fn [e] (and (in-sensor-range? e) (combat/alive? e))) (:enemies state)))

                  enemy-dist (when nearest-enemy
                               (math/distance [ax ay] (:pos nearest-enemy)))
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
                                 raw-path (pathfinding/try-search grid
                                            [(Math/round ^double ax) (Math/round ^double ay)]
                                            [(Math/round ^double ex) (Math/round ^double ey)])
                                 path (if raw-path (corridor/smooth-path grid raw-path) [])]
                             (assoc ally
                               :path (vec (or path []))
                               :state (if (seq path) :moving :idle)
                               :repath-timer 0.5))

                           (or
                             ;; repath toward player with offset
                             (and (> player-dist 3.0) (<= timer 0))
                             ;; repath towards player who's moved
                             (and (= (:state ally) :idle) (> player-moved-dist 2.0)))
                           (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                 ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                 raw-path (pathfinding/try-search grid [(Math/round ^double ax) (Math/round ^double ay)] [tx ty])
                                 path (if raw-path (corridor/smooth-path grid raw-path) [])]
                             (assoc ally
                               :path (vec (or path []))
                               :state (if (seq path) :moving :idle)
                               :repath-timer 0.75
                               :last-target [px py]))

                           :else
                           (assoc ally :repath-timer (max 0 timer)))

                         :defensive
                         (let [ally-to-player (math/distance [ax ay] [px py])
                               enemy-near-player? (and nearest-enemy
                                                    (<= (math/distance [px py] (:pos nearest-enemy))
                                                      3.0))]
                           (cond
                             ;; in melee range - fight
                             melee-range? (assoc ally :state :idle :path [])
                             ;; enemy near player - intercept
                             (and enemy-near-player? (<= timer 0))
                             (let [[ex ey] (:pos nearest-enemy)
                                   raw-path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [(Math/round ^double ex) (Math/round ^double ey)])
                                   path (if raw-path (corridor/smooth-path grid raw-path) [])]
                               (assoc ally
                                 :path (vec (or path []))
                                 :state (if (seq path) :moving :idle)
                                 :repath-timer 0.5
                                 :last-target [ex ey]))
                             ;; too far from player - return
                             (and (> ally-to-player 3.0) (<= timer 0))
                             ;; pathfind back to player
                             (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                   ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                   raw-path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [tx ty])
                                   path (if raw-path (corridor/smooth-path grid raw-path) [])]
                               (assoc ally
                                 :path (vec (or path []))
                                 :state (if (seq path) :moving :idle)
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
                             (let [raw-path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [(Math/round ^double ex) (Math/round ^double ey)])
                                   path (if raw-path (corridor/smooth-path grid raw-path) [])]
                               (assoc ally :path (vec (or path [])) :state (if (seq path) :moving :idle)
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
                                   raw-path (pathfinding/try-search grid
                                              [(Math/round ^double ax) (Math/round ^double ay)]
                                              [fx fy])
                                   path (if raw-path (corridor/smooth-path grid raw-path) [])]
                               (assoc ally :path (vec (or path [])) :state (if (seq path) :moving :idle)
                                 :repath-timer 0.75))
                             (<= timer 0)
                             ;; no enemy — follow player
                             (let [tx (+ (Math/round ^double px) (- (rand-int 3) 1))
                                   ty (+ (Math/round ^double py) (- (rand-int 3) 1))
                                   raw-path (pathfinding/try-search grid [(Math/round ^double ax) (Math/round ^double ay)] [tx ty])
                                   path (if raw-path (corridor/smooth-path grid raw-path) [])]
                               (assoc ally
                                 :path (vec (or path []))
                                 :state (if (seq path) :moving :idle)
                                 :repath-timer 0.75
                                 :last-target [px py]))
                             :else (assoc ally :repath-timer (max 0 timer)))))]
              (if (combat/alive? ally)
                (corridor/step-corridor-steering ally dt)
                ally))))
        allies))))

(defn tick-enemy-movement [state dt]
  (let [[px py] (:pos (:player state))
        grid (:grid state)]
    (assoc state :enemies
      (mapv (fn [enemy]
              (if (not (combat/alive? enemy))
                enemy  ;; dead/dying - don't touch
                (let [[ex ey] (:pos enemy)
                      timer (- (or (:repath-timer enemy) 0) dt)
                      enemy (if (<= timer 0)
                              (let [raw-path (pathfinding/try-search grid
                                               [(Math/round ^double ex) (Math/round ^double ey)]
                                               [(Math/round ^double px) (Math/round ^double py)])
                                    path (if raw-path (corridor/smooth-path grid raw-path) [])]
                                (assoc enemy
                                  :path (vec (or path []))
                                  :state (if (seq path) :moving :idle)
                                  :repath-timer 1.5))
                              (assoc enemy :repath-timer timer))]
                  (if (combat/alive? enemy)
                    (corridor/step-corridor-steering enemy dt)
                    enemy))))
        (:enemies state)))))

(defn tick-physics [{:keys [grid] :as state} dt]
  (-> state
    (update :player movement/step-physics grid dt)
    (assoc :allies (mapv (fn [ent] (movement/step-physics ent grid dt)) (:allies state)))
    (assoc :enemies (mapv (fn [ent] (movement/step-physics ent grid dt))  (:enemies state)))))

(defn tick-separation [state dt]
  (let [all-entities (into [(:player state)]
                       (concat (:allies state) (:enemies state)))
        ;; only include alive entities
        alive (filterv #(> (:hp % 0) 0) all-entities)
        spatial-hash (collision/build-spatial-hash alive)
        grid (:grid state)]
    (-> state
      (update :player
        (fn [p]
          (let [old-pos (:pos p)]
            (-> (first (collision/apply-separation [p] dt spatial-hash))
              (collision/clamp-to-walkable old-pos grid)))))
      (assoc :allies
        (mapv (fn [a]
                (if (> (:hp a 0) 0)
                  (let [old-pos (:pos a)]
                    (-> (first (collision/apply-separation [a] dt spatial-hash))
                      (collision/clamp-to-walkable old-pos grid)))
                  a))
          (:allies state)))
      (assoc :enemies
        (mapv (fn [e]
                (if (> (:hp e 0) 0)
                  (let [old-pos (:pos e)]
                    (-> (first (collision/apply-separation [e] dt spatial-hash))
                      (collision/clamp-to-walkable old-pos grid)))
                  e))
          (:enemies state))))))

(defn tick-bounds-check [state]
  (let [grid (:grid state)
        clamp-fn (partial grid/clamp grid)]
    (-> state
      (update-in [:player :pos] clamp-fn)
      (assoc :allies (mapv (fn [ally] (update ally :pos clamp-fn)) (:allies state)))
      (assoc :enemies (mapv (fn [enemy] (update enemy :pos clamp-fn)) (:enemies state))))))

(defn tick-combat [state dt]
  (let [{:keys [player allies enemies]}
        (combat/process-combat (:player state) (:allies state) (:enemies state) dt)]
    (assoc state :player player :allies allies :enemies enemies)))

(defn tick-score [state dt]
  (let [have-tombstones (count (filter (fn [e] (contains? e :tombstone))
                                 (:enemies state)))
        scrap-per-kill 10]
    (-> state
      ;; clear all tombstones
      (update :enemies (partial into [] (map (fn [e] (dissoc e :tombstone)))))
      (update-in [:score :kills] + have-tombstones)
      (update-in [:score :scrap] + (* scrap-per-kill have-tombstones))
      (update-in [:score :time-alive] + dt))))

(defn tick-spawning [state dt]
  (if (and (> (:hp (:player state)) 0)
        (< (count (:enemies state)) 100))
    (let [timer (- (:spawn-timer state) dt)]
      (if (<= timer 0)
        (let [pos (grid/random-edge-pos (:grid state))
              chassis (peep/random-chassis)
              drive-train (movement/random-drive-train)]
          (-> state
            (assoc :spawn-timer (:spawn-interval state))
            (update :enemies conj (peep/make-mech (peep/make-enemy pos) chassis drive-train))))
        (assoc state :spawn-timer timer)))
    state))

(defn tick-check-death [state]
  (when (<= (:hp (:player state)) 0)
    (reset! state/*screen :dead))
  state)

(defn tick [state {:keys [held controller dt] :as opts}]
  (-> state
    ;; current api for movement is decide movement and move them. should this change?
    (tick-player-input opts)
    (tick-ally-movement dt)
    (tick-enemy-movement dt)
    (tick-physics dt)
    (tick-separation dt)
    (tick-bounds-check)
    (tick-combat dt)
    (tick-score dt)
    (tick-spawning dt)
    (tick-check-death)))
