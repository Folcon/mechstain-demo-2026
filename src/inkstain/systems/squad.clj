(ns inkstain.systems.squad
  (:require [inkstain.config :as config]
            [inkstain.math :as math]
            [inkstain.systems.combat :as combat]))



(defn ring-slots
  "generate n evenly-spaced positions in a ring"
  [radius n]
  (mapv (fn [i]
          {:radius radius
           :angle (* (/ (* 2.0 Math/PI) n) i)})
    (range n)))

(defn arc-slots
  "generate n positions in an arc facing a facing-angle"
  [radius n facing-angle spread]
  (let [half-spread (/ spread 2.0)
        start (- facing-angle half-spread)]
    (mapv (fn [i]
            {:radius radius
             :angle (+ start (* (/ spread (max 1 (dec n))) i))})
      (range n))))

(defn assign-slots
  "assign entities to slot positions, each entity gets the nearest unclaimed slot
   returns {entity-id -> slot-position}"
  [entities slots anchor-pos]
  (let [slots (vec slots)
        resolved-slots (mapv (fn [{:keys [radius angle] :as _slot}]
                               [(+ (first anchor-pos) (* radius (Math/cos angle)))
                                (+ (second anchor-pos) (* radius (Math/sin angle)))])
                         slots)]
    (loop [remaining
           (sort-by
             (fn [e]
               ;; assign closest entities first to reduce crossing
               (let [[ex ey] (:pos e)
                     dists (map (fn [[sx sy]]
                                  (+ (* (- sx ex) (- sx ex))
                                    (* (- sy ey) (- sy ey))))
                             resolved-slots)]
                 (apply min dists)))
             entities)
           claimed #{}
           assignments {}]
      (if (empty? remaining)
        assignments
        (let [entity (first remaining)
              [ex ey] (:pos entity)
              ;; find nearest unclaimed slot
              best (reduce
                     (fn [best idx]
                       (if (contains? claimed idx)
                         best
                         (let [[sx sy] (resolved-slots idx)
                               d (+ (* (- sx ex) (- sx ex))
                                   (* (- sy ey) (- sy ey)))]
                           (if (or (nil? best) (< d (:dist best)))
                             {:idx idx :dist d}
                             best))))
                     nil
                     (range (count resolved-slots)))
              slot-idx (:idx best)]
          (recur (rest remaining)
            (conj claimed slot-idx)
            (assoc assignments (:id entity) (slots slot-idx))))))))

(defn needs-reassignment?
  "check if allies need slot reassignment"
  [allies tactical-mode]
  ;; any ally has no slot
  (or
    (some #(nil? (:slot %)) allies)
    (some #(not= tactical-mode (:tactical-mode %)) allies)))

(defn update-ally-slots
  "assign or reassign ally :slot positions based on tactical mode"
  [allies anchor-pos tactical-mode enemies]
  (let [alive-allies (filterv combat/alive? allies)
        n (count alive-allies)]
    (if (zero? n)
      allies
      (let [{:keys [follow-radius defensive-radius surround-radius
                    hold-radius hold-spread reassign-threshold]} config/squad-slot-config

            ;; do we need to reassign?
            reassign? (needs-reassignment? alive-allies tactical-mode)

            ;; if no reassignment needed, return as-is
            alive-allies
            (if (not reassign?)
              alive-allies
              (let [;; TODO: pull sensor range from individual mechs,
                    ;;   pool sensor range (all mechs share sensor range)
                    ;;   or allow sensor range networks (mechs share sensor range with lance or partners)
                    sensor-range config/sensor-range
                    [ax ay] anchor-pos
                    in-sensor-range? (fn [e] (< (math/distance [ax ay] (:pos e)) sensor-range))
                    nearest-enemy (when (seq enemies)
                                    (combat/find-nearest-hostile anchor-pos
                                      (filter
                                        (fn [e] (and (in-sensor-range? e) (combat/alive? e)))
                                        enemies)))

                    [slots anchor-type anchor-id]
                    (case tactical-mode
                      :aggressive
                      (if nearest-enemy
                        ;; surround the nearest enemy
                        [(ring-slots surround-radius n) :enemy (:id nearest-enemy)]
                        ;; no enemies, ring around player
                        [(ring-slots follow-radius n) :player nil])

                      :defensive
                      [(ring-slots defensive-radius n) :player nil]

                      :hold
                      (let [facing (if nearest-enemy
                                     (let [[ax ay] anchor-pos
                                           [ex ey] (:pos nearest-enemy)]
                                       (Math/atan2 (- ey ay) (- ex ax)))
                                     0.0)]
                        [(arc-slots hold-radius n facing hold-spread) :player nil])

                      :flank
                      (if nearest-enemy
                        (let [[ax ay] anchor-pos
                              [ex ey] (:pos nearest-enemy)
                              facing (Math/atan2 (- ey ay) (- ex ax))]
                          ;; arc on far side of enemy, 270 degree spread
                          ;;   center faces AWAY from player
                          [(arc-slots surround-radius n
                             (+ facing Math/PI)
                             (* Math/PI 1.5))
                           :enemy (:id nearest-enemy)])
                        ;; no enemies, fall back to follow ring
                        [(ring-slots follow-radius n) :player nil])

                      ;; default fallback
                      [(ring-slots follow-radius n) :player nil])

                    dynamic-anchor-pos (if (= anchor-type :enemy)
                                         (:pos nearest-enemy)
                                         anchor-pos)

                    ;; assign each ally to a slot
                    assignments (assign-slots alive-allies slots dynamic-anchor-pos)]

                ;; attach slot to each ally
                (mapv (fn [ally]
                        (if-let [slot-pos (get assignments (:id ally))]
                          (assoc ally :slot (merge slot-pos
                                              {:type :dynamic
                                               :anchor-type anchor-type}
                                              (when anchor-id
                                                {:anchor-id anchor-id}))
                            :tactical-mode tactical-mode)
                          ally))
                  alive-allies)))

            ;; merge back, replace alive allies, keep dead ones unchanged
            alive-ids (into #{} (map :id) alive-allies)
            alive-by-id (into {} (map (fn [a] [(:id a) a])) alive-allies)]
        (mapv (fn [ally]
                (if (contains? alive-ids (:id ally))
                  (get alive-by-id (:id ally))
                  ally))
          allies)))))

(defn resolve-slot [entity player-pos enemies]
  (when-let [slot (:slot entity)]
    (case (:type slot)
      :static (:pos slot)

      :dynamic
      (let [anchor-pos (case (:anchor-type slot)
                         :player player-pos
                         :enemy (some #(when (= (:id %) (:anchor-id slot))
                                         (:pos %))
                                  enemies)
                         player-pos)
            r (:radius slot)
            a (:angle slot)]
        (when anchor-pos
          [(+ (first anchor-pos) (* r (Math/cos a)))
           (+ (second anchor-pos) (* r (Math/sin a)))]))
      nil)))

(defn resolve-target [entity player-pos enemies]
  (if-let [chase (:chase entity)]
    ;; chasing, go directly to enemy
    (some #(when (= (:id %) (:anchor-id chase)) (:pos %)) enemies)
    ;; resolve formation slot
    (resolve-slot entity player-pos enemies)))
