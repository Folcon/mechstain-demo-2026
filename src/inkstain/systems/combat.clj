(ns inkstain.systems.combat)




(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn find-nearest-hostile [pos hostiles]
  (when (seq hostiles)
    (apply min-key #(distance pos (:pos %)) hostiles)))

(defn try-attack [attacker target dt]
  (let [dist (distance (:pos attacker) (:pos target))
        cooldown (max 0 (- (:attack-cooldown attacker 0) dt))]
    (if (and (<= dist (:attack-range attacker 1.5))
          (<= cooldown 0))
      ;; in range and ready: deal damage
      {:attacker (assoc attacker :attack-cooldown (:attack-speed attacker
                                                   0.5))
       :target (-> target
                 (update :hp - (:attack-damage attacker 10))
                 (assoc :hit-timer 0.15))}  ;; flash for 150ms
      ;; not ready or not in range: just tick cooldown
      {:attacker (assoc attacker :attack-cooldown cooldown)
       :target target})))

(defn update-hit-timers [entities dt]
  (mapv (fn [e]
          (if (pos? (:hit-timer e 0))
            (update e :hit-timer - dt)
            e))
    entities))

(defn alive? [entity]
  (> (:hp entity) 0))

(defn dying? [entity]
  (and (not (alive? entity))
    (> (:death-timer entity 0) 0)))

(defn dead? [entity]
  (and (not (alive? entity))
    (<= (:death-timer entity 0) 0)))

(defn start-dying [entity]
  (assoc entity :death-timer 0.5 :state :dying :tombstone true))  ;; 500ms death animation

(defn update-death-timers [entities dt]
  (mapv (fn [e]
          (if (= :dying (:state e))
            (update e :death-timer - dt)
            e))
    entities))

(defn process-combat [player allies enemies dt]
  (let [;; tick down cooldowns and hit timers
        player  (update player :hit-timer #(max 0 (- (or % 0) dt)))
        allies  (update-hit-timers allies dt)
        enemies (update-hit-timers enemies dt)

        ;; update death animations, remove fully dead
        allies  (update-death-timers allies dt)
        enemies (update-death-timers enemies dt)
        allies  (vec (remove dead? allies))
        enemies (vec (remove dead? enemies))

        ;; allies attack nearest enemy
        {allies :attackers enemies :targets}
        (reduce
          (fn [{:keys [attackers targets]} ally]
            (if (or (not (alive? ally)) (= :dying (:state ally)))
              {:attackers (conj attackers ally) :targets targets}
              (let [nearest (find-nearest-hostile (:pos ally) (filter alive?
                                                                targets))]
                (if (and nearest (<= (distance (:pos ally) (:pos nearest))
                                   (:attack-range ally 1.5)))
                  (let [{:keys [attacker target]} (try-attack ally nearest dt)
                        target (if (and (not (alive? target))
                                     (not= :dying (:state target)))
                                 (start-dying target)
                                 target)
                        targets (mapv #(if (= (:id %) (:id target)) target %)
                                  targets)]
                    {:attackers (conj attackers attacker) :targets targets})
                  {:attackers (conj attackers (update ally :attack-cooldown
                                                #(max 0 (- (or % 0) dt))))
                   :targets targets}))))
          {:attackers [] :targets enemies}
          allies)

        ;; enemies attack nearest ally or player
        all-friendlies (into [player] (filter alive? allies))
        {enemies :attackers [player & updated-allies] :targets}
        (reduce
          (fn [{:keys [attackers targets]} enemy]
            (if (or (not (alive? enemy)) (= :dying (:state enemy)))
              {:attackers (conj attackers enemy) :targets targets}
              (let [nearest (find-nearest-hostile (:pos enemy) (filter alive?
                                                                 targets))]
                (if (and nearest (<= (distance (:pos enemy) (:pos nearest))
                                   (:attack-range enemy 1.5)))
                  (let [{:keys [attacker target]} (try-attack enemy nearest
                                                    dt)
                        target (if (and (not (alive? target))
                                     (not= :dying (:state target)))
                                 (start-dying target)
                                 target)
                        targets (mapv #(if (= (:id %) (:id target)) target %)
                                  targets)]
                    {:attackers (conj attackers attacker) :targets targets})
                  {:attackers (conj attackers (update enemy :attack-cooldown
                                                #(max 0 (- (or % 0) dt))))
                   :targets targets}))))
          {:attackers [] :targets all-friendlies}
          enemies)

        ;; reconcile allies from updated targets
        allies (vec (or updated-allies []))]

    {:player player
     :allies allies
     :enemies enemies}))
