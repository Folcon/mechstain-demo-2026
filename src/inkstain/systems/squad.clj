(ns inkstain.systems.squad
  (:require [inkstain.config :as config]
            [inkstain.math :as math]
            [inkstain.systems.combat :as combat]))



(defn ring-slots
  "generate n evenly-spaced positions in a ring around anchor"
  [anchor radius n]
  (let [[ax ay] anchor]
    (mapv (fn [i]
            (let [angle (* (/ (* 2.0 Math/PI) n) i)]
              [(+ ax (* radius (Math/cos angle)))
               (+ ay (* radius (Math/sin angle)))]))
      (range n))))

(defn arc-slots
  "generate n positions in an arc facing a facing-angle, centered on anchor"
  [anchor radius n facing-angle spread]
  (let [[ax ay] anchor
        half-spread (/ spread 2.0)
        start (- facing-angle half-spread)]
    (mapv (fn [i]
            (let [angle (+ start (* (/ spread (max 1 (dec n))) i))]
              [(+ ax (* radius (Math/cos angle)))
               (+ ay (* radius (Math/sin angle)))]))
      (range n))))

(defn assign-slots
  "assign entities to slot positions, each entity gets the nearest unclaimed slot
   returns {entity-id -> slot-position}"
  [entities slots]
  (let [slots (vec slots)]
    (loop [remaining
           (sort-by
             (fn [e]
               ;; assign closest entities first to reduce crossing
               (let [[ex ey] (:pos e)
                     dists (map (fn [[sx sy]]
                                  (+ (* (- sx ex) (- sx ex))
                                    (* (- sy ey) (- sy ey))))
                             slots)]
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
                         (let [[sx sy] (slots idx)
                               d (+ (* (- sx ex) (- sx ex))
                                   (* (- sy ey) (- sy ey)))]
                           (if (or (nil? best) (< d (:dist best)))
                             {:idx idx :dist d}
                             best))))
                     nil
                     (range (count slots)))
              slot-idx (:idx best)]
          (recur (rest remaining)
            (conj claimed slot-idx)
            (assoc assignments (:id entity) (slots slot-idx))))))))

(defn needs-reassignment?
  "check if allies need slot reassignment"
  [allies anchor-pos reassign-threshold]
  (or
    ;; any ally has no slot
    (some #(nil? (:slot %)) allies)
    ;; anchor moved significantly from when slots were assigned
    (some (fn [ally]
            (when-let [slot-anchor (get-in ally [:slot :anchor])]
              (> (math/distance anchor-pos slot-anchor) reassign-threshold)))
      allies)))

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
            reassign? (needs-reassignment? alive-allies anchor-pos reassign-threshold)

            ;; if no reassignment needed, return as-is
            alive-allies
            (if (not reassign?)
              alive-allies
              (let [sensor-range config/sensor-range
                    nearest-enemy (when (seq enemies)
                                    (combat/find-nearest-hostile anchor-pos
                                      (filter combat/alive? enemies)))

                    slots (case tactical-mode
                            :aggressive
                            (if nearest-enemy
                              ;; surround the nearest enemy
                              (ring-slots (:pos nearest-enemy) surround-radius n)
                              ;; no enemies, ring around player
                              (ring-slots anchor-pos follow-radius n))

                            :defensive
                            (ring-slots anchor-pos defensive-radius n)

                            :hold
                            (let [facing (if nearest-enemy
                                           (let [[ax ay] anchor-pos
                                                 [ex ey] (:pos nearest-enemy)]
                                             (Math/atan2 (- ey ay) (- ex ax)))
                                           0.0)]
                              (arc-slots anchor-pos hold-radius n facing hold-spread))

                            :flank
                            (if nearest-enemy
                              (let [[ax ay] anchor-pos
                                    [ex ey] (:pos nearest-enemy)
                                    facing (Math/atan2 (- ey ay) (- ex ax))]
                                ;; arc on far side of enemy, 270 degree spread
                                ;;   center faces AWAY from player
                                (arc-slots (:pos nearest-enemy) surround-radius n
                                  (+ facing Math/PI)
                                  (* Math/PI 1.5)))
                              ;; no enemies, fall back to follow ring
                              (ring-slots anchor-pos follow-radius n))

                            ;; default fallback
                            (ring-slots anchor-pos follow-radius n))

                    ;; assign each ally to a slot
                    assignments (assign-slots alive-allies slots)]

                ;; attach slot to each ally
                (mapv (fn [ally]
                        (if-let [slot-pos (get assignments (:id ally))]
                          (assoc ally :slot {:pos slot-pos :anchor anchor-pos})
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
