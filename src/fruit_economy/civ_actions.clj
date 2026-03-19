(ns fruit-economy.civ-actions
  (:require [fruit-economy.land :as land :refer [log-history]]
            [fruit-economy.rand :as rand]
            [fruit-economy.economy :as economy]
            [fruit-economy.data.core :as data]
            [fruit-economy.utils :refer [clamp]]))


(defn grow-pop' [world-db {:fruit-economy.civ/keys [power name] :as _civ}]
  (-> (data/log-history (str name " is growing"))
    (data/upsert-civ name {:fruit-economy.civ/power (+ power (* power (/ (rand/roll 4) 10)))})))

(defn grow-pop [land-data {:fruit-economy.civ/keys [power name] :as _civ}]
  (-> (log-history land-data (str name " is growing"))
    (update-in [::land/civ-name->civ name :fruit-economy.civ/power] + (* power (/ (rand/roll 4) 10)))))

(defn expand-territory [{::land/keys [width height] :as land-data} {:fruit-economy.civ/keys [power dead territory name] :as civ}]
  (println civ "\n  " dead (pr-str territory))
  (if (and (> power 10) (or (not dead) (seq territory)))
    (as-> land-data $
      (log-history $ (str name " is trying to expand"))
      (update-in $ [::land/civ-name->civ name :fruit-economy.civ/power] - 10)
      (reduce
        (fn [{::land/keys [terrain civ-name->civ area->civ-name area->manor] :as land} _n]
          (let [{:fruit-economy.civ/keys [tech-level home-biome] :as civ} (get civ-name->civ name)
                [attempt-x attempt-y] (rand-nth (vec territory))
                _ (println :attempt [attempt-x attempt-y] [width height] [(clamp (dec attempt-x) {:mx (dec height)}) (clamp (inc attempt-x) {:mx (dec height)})])
                ax (rand/rand-int-between
                     (clamp (dec attempt-x) {:mx (dec width)})
                     (clamp (inc attempt-x) {:mx (dec width)}))
                ay (rand/rand-int-between
                     (clamp (dec attempt-y) {:mx (dec height)})
                     (clamp (inc attempt-y) {:mx (dec height)}))
                attempt-biome (get-in terrain [ay ax])]
            (println :attempt-loc attempt-biome territory [ax ay] (contains? territory [ax ay]))
            ;; cannot expand into water
            (println :tech-level tech-level home-biome)
            (if-not (or (= attempt-biome :ocean) (contains? territory [ax ay]))
              (let [;; base 10% success chance, max +70% from technology
                    chance (cond-> (+ 10 (min 70 (* 5 tech-level)))
                             ;; bonus chance for expansion if attempting to expand into base biome
                             (= attempt-biome home-biome)
                             (+ 20)

                             ;; spreading into occupied squares is harder
                             (get area->civ-name [ax ay])
                             (- 5 (Math/ceil (/ (get-in civ-name->civ [(get area->civ-name [ax ay]) :fruit-economy.civ/tech-level]) 10)))

                             ;; TODO: Double check area->manor works!!
                             (get area->manor [ax ay])
                             (- 5 (Math/ceil (/ (get-in area->manor [[ax ay] :fruit-economy.civ/level]) 2)))

                             ;; mountains make it hard to spread
                             (= (get-in terrain [ay ax]) :mountain)
                             (- 5)

                             ;; grasslands easy to spread
                             (= (get-in terrain [ay ax]) :grassland)
                             (+ 5))
                    roll-result (rand/roll 100)
                    existing-claim (get area->civ-name [ax ay])]
                (println "<" :roll-result roll-result :chance chance)
                ;; lower rolls are better
                (if (< roll-result chance)
                  (cond-> (log-history land (str "Expanding " name " at " [ax ay] (when existing-claim (str " replacing " existing-claim))))
                    ;; before territory chances hands, update economy sources
                    :always
                    (economy/assign-source->civ civ [ax ay])

                    ;; overwrite existing territorial claim
                    existing-claim
                    (update-in [::land/civ-name->civ existing-claim :fruit-economy.civ/territory] disj [ax ay])
                    ;; TODO: Double check area->manor works!!
                    ;; manors change hands
                    ;; if self.homeworld.settlements[ay][ax]:
                    ;;   self.homeworld.settlements[ay][ax].history.append(self)

                    :always
                    (assoc-in [::land/area->civ-name [ax ay]] name)

                    :always
                    (update-in [::land/civ-name->civ name :fruit-economy.civ/territory] conj [ax ay]))
                  land))
              land)))
        $
        (range (rand/rand-int-between 2 5))))
    (log-history land-data (str name " could not expand and needs 11 [" (int power) "]"))))

(defn improve-tech-level [land-data {:fruit-economy.civ/keys [power tech-level name] :as _civ}]
  (if (> power 20)
    (let [tech-level' (+ tech-level (/ (rand/roll 10) 10))]
      (-> (log-history land-data (str name " is improving their technology"))
        (update-in [::land/civ-name->civ name :fruit-economy.civ/power] - 20)
        (assoc-in [::land/civ-name->civ name :fruit-economy.civ/tech-level] tech-level')
        ;; developing more advanced architecture
        #_(cond->
            (and
              (> tech-level' (nth tech-stages current-stage))
              (< current-stage (dec (count tech-stages))))
            (->
              (update-in [::land/civ-name->civ name :fruit-economy.civ/current-stage] inc)
              (update-in [::land/civ-name->civ name :fruit-economy.civ/architecture] conj (rand-nth shapes))))))
    (log-history land-data (str name " could not improve their technology and needs 21 [" (int power) "]"))))
