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
            [inkstain.systems.squad :as squad]
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
        grid (:grid state)
        allies (squad/update-ally-slots
                 allies [px py]
                 (:tactical-mode state)
                 (:enemies state))]
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
                           (let [[tx ty] (get-in ally [:slot :pos])
                                 tx (Math/round ^double tx)
                                 ty (Math/round ^double ty)
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
                             (let [[tx ty] (get-in ally [:slot :pos])
                                   tx (Math/round ^double tx)
                                   ty (Math/round ^double ty)
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
                         ;; Flanking has a complex logic chain, so I'll document it here for reference
                         ;;  1. melee range -> stop and fight
                         ;;  2. enemy nearby -> check distance to assigned flank slot, if within 1.5 tiles
                         ;;    peep is in position and should close in on the enemy directly with shorter repath timer
                         ;;    of 0.5s, if they're not in position yet, pathfind to the flank slot with longer timer of 0.75s
                         ;;  3. no enemy -> pathfind to slot around the player
                         ;;  4. tick timer down
                         (cond
                           melee-range? (assoc ally :state :idle :path [])

                           ;; enemy nearby - move to flank position or close in
                           (and enemy-nearby? (<= timer 0))
                           (let [[sx sy] (get-in ally [:slot :pos])
                                 ;; if close to flank slot, close in on enemy directly
                                 slot-dist (when (and sx sy)
                                             (math/distance [ax ay] [sx sy]))
                                 ;; within 1.5 tiles of slot is in position so attack directly
                                 in-position? (and slot-dist (< slot-dist 1.5))
                                 ;; if in position, close in on enemy directly
                                 ;;   otherwise, move to flank slot
                                 [tx ty] (if in-position?
                                           (:pos nearest-enemy)
                                           (get-in ally [:slot :pos]))
                                 tx (Math/round ^double tx)
                                 ty (Math/round ^double ty)
                                 raw-path (pathfinding/try-search grid
                                            [(Math/round ^double ax) (Math/round ^double ay)]
                                            [tx ty])
                                 path (if raw-path (corridor/smooth-path grid raw-path) [])]
                             (assoc ally
                               :path (vec (or path []))
                               :state (if (seq path) :moving :idle)
                               :repath-timer (if in-position? 0.5 0.75)))
                           (<= timer 0)
                           ;; no enemy - follow player
                           (let [[tx ty] (get-in ally [:slot :pos])
                                 tx (Math/round ^double tx)
                                 ty (Math/round ^double ty)
                                 raw-path (pathfinding/try-search grid [(Math/round ^double ax) (Math/round ^double ay)] [tx ty])
                                 path (if raw-path (corridor/smooth-path grid raw-path) [])]
                             (assoc ally
                               :path (vec (or path []))
                               :state (if (seq path) :moving :idle)
                               :repath-timer 0.75
                               :last-target [px py]))
                           :else (assoc ally :repath-timer (max 0 timer))))]
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
    (input/swap-focus! :dead))
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
