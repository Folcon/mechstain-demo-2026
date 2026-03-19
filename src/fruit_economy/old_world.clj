(ns fruit-economy.old-world
  (:require [fruit-economy.noise :refer [coord->noise simplex-noise-fn]]
            [fruit-economy.colour :refer [colour]]))


(def width 5 #_50)
(def height 2 #_20)

(comment
  (let [n 0
        octaves [2 4 8] lacunarity [1.25 2 4]]
    (coord->noise simplex-noise-fn [10 10] {:seed 1 :octave (nth octaves n) :lacunarity (nth lacunarity n) :persistence 0.5 :scale [1 1] :normalise? false})))

(comment
  (let [n 0
        octaves [2 4 8] lacunarity [1 1.25 2 4]]
    (coord->noise simplex-noise-fn [10 10] {:seed 1 :octave (nth octaves n) :lacunarity (nth lacunarity n) :persistence 0.5 :scale [1 1] :normalise? true})))


;; TODO: Turn these into record based protocol like thing,
;;   ie you have a noise-map, and call [x y] into it and it does the computation with the values it has
(defn simplex-noise-map [width height {:keys [seed octave scale low high] :or {scale 1 low 0 high 1}}]
  (let [norm (fn [v size] (/ (float v) size))]
    (into []
      (for [y (range height)]
        (into []
          (for [x (range width)]
            (coord->noise simplex-noise-fn [(norm x width) (norm y height)]
              {:seed seed :octave octave :lacunarity 1
               :persistence 0.5 :scale [scale scale]
               :normalise? true :low -1 :high 1})))))))

(comment
  (simplex-noise-map 2 3 {:seed 1 :octave 1}))

(defn grey-noise-map
  "greyscale map"
  [w h {:keys [seed octaves]}]
  (let [norm (fn [v size] (/ (float v) size))]
    (into []
      (for [y (range h)]
        (into []
          (for [x (range w)]
            (int (* 256 (coord->noise simplex-noise-fn [(norm x width) (norm y height)]
                          {:seed seed :octave octaves :lacunarity 1
                           :persistence 0.5 :scale [0.007 0.007]
                           :normalise? true :low 0 :high 1})))))))))

(defn make-noise-map [width height seed-size octaves]
  (let [[o0 o1 o2] octaves]
    {:width width :height height
     :noise-coll
     [(simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o0})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o1})
      (simplex-noise-map width height {:seed (inc (rand-int seed-size)) :octave o2})]}))

(defn make-temp-noise-map [width height]
  (make-noise-map width height 10000 [2 4 88]))

(comment
  (make-temp-noise-map 2 3))

(defn make-elev-noise-map [width height]
  (make-noise-map width height 10000 [4 10 20]))

(defn messy-noise [noise-coll [x y]]
  (let [[n0 n1 n2] noise-coll]
    (+
      (get-in n0 [y x])
      (* (get-in n1 [y x]) 0.5)
      (* (get-in n2 [y x]) 0.25))))

(defn process-noise-map [{:keys [width height noise-coll] :as _noise-map} val-mod]
  (into []
    (map (fn [row]
           (mapv (fn [coord]
                   (+ (messy-noise noise-coll coord) val-mod))
               row)))
    (for [y (range height)]
      (into []
        (for [x (range width)]
          [x y])))))

;; TODO: Spec world, civ, settlement and validate
(defn make-world [name width height {:keys [temp-mod elev-mod temp-noise elev-noise base-biome] :or {temp-mod 0.1 elev-mod 0 base-biome :ocean}}]
  (let [temp-noise (or temp-noise (make-temp-noise-map width height))
        elev-noise (or elev-noise (make-elev-noise-map width height))]
    {:name name
     :temp-noise temp-noise :elev-noise elev-noise
     :temp (process-noise-map temp-noise temp-mod)
     :elev (process-noise-map elev-noise elev-mod)
     :base-biome base-biome
     :terrain (vec (repeat height (vec (repeat width base-biome))))
     :civ-map {}
     :settlements {}
     :ruins (vec (repeat height (vec (repeat width []))))
     :width width :height height :sea-level (rand-nth (range 0.001 0.009 0.001))
     :curr-civ 0
     :civs {}
     :civ-missives []
     :civ-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()-_=+[{]}\\|;:,<.>/?"}))

(defn generate-world [{:keys [width height sea-level] :as world-data}]
  (reduce
    (fn [world y]
      (reduce
        (fn [{:keys [temp elev] :as world} x]
          (let [local-temp (get-in temp [y x])
                local-elev (get-in elev [y x])]
            (cond
              (and (>= local-elev sea-level) (< local-elev 0.1))
              (assoc-in world [:terrain y x] :beach)

              (and (>= local-elev 0.4) (< local-temp -0.2))
              (assoc-in world [:terrain y x] :snow-mountain)

              (>= local-elev 0.4)
              (assoc-in world [:terrain y x] :mountain)

              ;; temperate region
              (> local-elev sea-level)
              (cond
                (< local-temp -0.2)
                (assoc-in world [:terrain y x] :snow)

                (< local-temp 0)
                (assoc-in world [:terrain y x] :tundra)

                (< local-temp 0.1)
                (assoc-in world [:terrain y x] :grassland)

                (< local-temp 0.2)
                (assoc-in world [:terrain y x] :forest)

                (< local-temp 0.3)
                (assoc-in world [:terrain y x] :jungle)

                :else
                (assoc-in world [:terrain y x] :desert))

              :else
              world)))
        world
        (range width)))
    world-data
    (range height)))

(defn render-tile-colour [terrain]
  (condp = terrain
    :beach (colour 230 236 172) #_(rand-nth [(colour 117 139 171) (colour 230 236 172)])
    :desert (colour 246 244 118)
    :forest (colour 64 88 37)
    :grassland (colour 78 138 53)
    :jungle (colour 66 108 40)
    :mountain (colour 73 66 52)
    :snow-mountain (colour 86 82 73)
    :ocean (rand-nth [(colour 87 119 197) (colour 87 102 153) (colour 87 102 153)])
    :snow (colour 247 246 247)
    :tundra (colour 153 153 155)
    (colour 0 0 0)))

(defn render-tile-str [terrain]
  (condp = terrain
    :beach "b"
    :desert "d"
    :forest "f"
    :grassland "g"
    :jungle "j"
    :mountain "m"
    :snow-mountain "M"
    :ocean "~"
    :snow "s"
    :tundra "t"
    (str terrain)))

(defn render-settlement-colour [{:keys [level] :as _settlement}]
  (condp > level
    50 (colour 78 138 55)
    30 (colour 230 236 170)
    10 (colour 0 255 255)
    (colour 87 117 150)))

(defn roll [size]
  (inc (rand-int size)))

(def shapes
  (into [] cat [(repeat 5 "rectangle") (repeat 3 "square") (repeat 3 "circle") ["rhombus" "cross" "triangle" "hexagon" "octagon"]]))

(def milestones [1 2 5 10 20 50 100 200])

(defn make-civ [name symbol origin home-world-name terrain ancestor]
  (let [[x y] origin]
    (merge
      {:name name #_self.language.brute_force_translate
       :symbol symbol
       :tint (colour (rand-int 256) (rand-int 256) (rand-int 256))
       :territory #{origin}
       :home-world home-world-name
       :home-biome terrain

       ;; stats
       :instability -10  ;; low starting instability to ensure early expansion/survival
       :power (reduce + (repeatedly 3 #(roll 10)))
       :power-base 1
       :tech-level 0
       :dead false
       ;; internal priorities
       :priorities (into [] (comp (map (fn [priority] (repeat (roll 8) priority))) cat) [:tech-development :territorial-expansion :population-growth :territorial-stabilisation :settlement-construction])

       ;; diplomatic profile
       :profile {:friendliness (roll 100)
                 :trustworthiness (roll 100)
                 :fearfulness (roll 100)
                 :reputation 0}

       ;; diplomatic relationships
       :relationships {}
       :age 0

       ;; architecture
       :architecture [(rand-nth shapes)]
       :current-milestone 0}
      (when ancestor
        {:ancestor ancestor}))))

(declare civ-expand-territory)

(defn spawn-civ [{:keys [name terrain curr-civ civ-letters] :as world-data} x y {:keys [parent expand?]}]
  (if (<= curr-civ (dec (count civ-letters)))
    (let [symbol (nth civ-letters curr-civ)
          civ-name (str "Civ " symbol "+" curr-civ)
          new-civ (make-civ civ-name symbol [x y] name (get-in terrain [y x]) parent)]
      (println (str "Spawning new civ at " x " " y))
      (-> world-data
        (assoc-in [:civ-map [x y]] civ-name)
        (update :civs assoc civ-name new-civ)
        (update :curr-civ inc)
        (cond->
          expand?
          (as-> $ (civ-expand-territory $ (get-in $ [:civs civ-name]))))))
    (do
      (println (str "Tried to spawn new civ at " x " " y " ran out of letters"))
      world-data)))

(defn try-spawn-new-civs [{:keys [width height] :as world-data} n-attempts]
  (reduce
    (fn [world _n]
      (let [x (rand-int width)
            y (rand-int height)
            target (get-in world [:terrain y x])]
        (println _n target)
        (if (not= target :ocean)
          (spawn-civ world x y {})
          world)))
    world-data
    (range n-attempts)))

(defn rand-between
  "Returns a random floating point number between 0 (inclusive) and
   n (default 1) (exclusive).
   If two parameters are provided, returns a random floating point number between aand b."
  {:added "1.0"    :static true}
  ([] (. Math (random)))
  ([n] (* n (rand-between)))
  ([a b] (+ a (rand-between (- b a)))))

(defn rand-int-between
  "Returns a random integer between 0 (inclusive) and n (exclusive).
  If two parameters are provided, returns a random integer between a and b."
  {:added "1.0"    :static true}
  ([n] (int (rand-between n)))
  ([a b] (int (rand-between a b))))

(defn clamp [val {:keys [mn mx] :or {mn 0 mx 1}}]
  (max mn (min val mx)))

(defn civ-expand-territory [{:keys [width height] :as world-data} {:keys [dead territory name] :as civ}]
  (println civ "\n  " dead (pr-str territory))
  (if (or (not dead) (seq territory))
    (reduce
      (fn [{:keys [terrain civs civ-map settlements] :as world} _n]
        (let [{:keys [tech-level home-biome] :as civ} (get civs name)
              [attempt-x attempt-y] (rand-nth (vec territory))
              _ (println :attempt [attempt-x attempt-y] [width height] [(clamp (dec attempt-x) {:mx (dec height)}) (clamp (inc attempt-x) {:mx (dec height)})])
              ax (rand-int-between
                   (clamp (dec attempt-x) {:mx (dec width)})
                   (clamp (inc attempt-x) {:mx (dec width)}))
              ay (rand-int-between
                   (clamp (dec attempt-y) {:mx (dec height)})
                   (clamp (inc attempt-y) {:mx (dec height)}))
              attempt-biome (get-in terrain [ay ax])]
          (println :attempt-loc attempt-biome territory [ax ay] (contains? territory attempt-biome))
          ;; cannot expand into water
          (println :tech-level tech-level home-biome)
          (if-not (or (= attempt-biome :ocean) (contains? territory [ax ay]))
            (let [;; base 10% success chance, max +70% from technology
                  chance (cond-> (+ 10 (min 70 (* 5 tech-level)))
                           ;; bonus chance for expansion if attempting to expand into base biome
                           (= attempt-biome home-biome)
                           (+ 20)

                           ;; spreading into occupied squares is harder
                           (get civ-map [ax ay])
                           (- 5 (Math/ceil (/ (get-in civs [(get civ-map [ax ay]) :tech-level]) 10)))

                           ;; TODO: Double check settlements works!!
                           (get settlements [ax ay])
                           (- 5 (Math/ceil (/ (get-in settlements [[ax ay] :level]) 2)))

                           ;; mountains make it hard to spread
                           (= (get-in terrain [ay ax]) :mountain)
                           (- 5)

                           ;; grasslands easy to spread
                           (= (get-in terrain [ay ax]) :grassland)
                           (+ 5))
                  roll-result (roll 100)]
              (println :chance chance roll-result)
              ;; lower rolls are better
              (if (< roll-result chance)
                (do
                  (println (str "Expanding " name " at " [ax ay] " replacing " (get civ-map [ax ay])))
                  (cond-> world
                    ;; overwrite existing territorial claim
                    (get civ-map [ax ay])
                    (update-in [:civs (get civ-map [ax ay]) :territory] disj [ax ay])
                    ;; TODO: Double check settlements works!!
                    ;; settlements change hands
                    ;; if self.homeworld.settlements[ay][ax]:
                    ;;   self.homeworld.settlements[ay][ax].history.append(self)

                    :always
                    (assoc-in [:civ-map [ax ay]] name)

                    :always
                    (update-in [:civs name :territory] conj [ax ay])))
                world))
            world)))
      world-data
      (range (rand-int-between 2 5)))
    world-data))

(defn make-settlement [name home-world-name origin level founding-civ tech-level]
  (let [[x y] origin]
    {:name name
     :home-world home-world-name
     :x x :y y :level level :history [founding-civ]
     :architecture-level tech-level}))

(defn settlement-upgrade [world-data loc]
  (update-in world-data [:settlements loc :level] inc))

(defn civ-build-settlement [{:keys [width height settlements terrain] :as world-data} {:keys [dead territory age home-world name tech-level] :as civ}]
  (println civ "\n  " dead (pr-str territory))
  (if (or (not dead) (seq territory))
    (let [loc (rand-nth (vec territory))
          build-target (get settlements loc)]
      (if build-target
        (-> world-data
          (settlement-upgrade loc)
          (update-in [:civs name :power] - 5))
        (let [build-biome (get terrain loc)
              ;; base 10% chance of building success
              chance (cond-> 10
                       ;; hard to build on mountains
                       (= build-biome :mountain)
                       (- 5)

                       ;; easy to build on grassland
                       (= build-biome :grassland)
                       (- 10))
              roll-result (roll 100)]
          (println :chance chance roll-result)
          ;; lower rolls are better
          (if (< roll-result chance)
            (let [settlement-name (str "Settlement Established " age)
                  settlement (make-settlement settlement-name home-world loc 0 name 0)]
              (println (str name " has established a settlement " settlement-name " at " loc))
              (-> world-data
                (update-in [:civs name :power] - 5)
                (assoc-in [:settlements loc] settlement)))
            (do
              (println (str name " attempted to start a settlement, but it failed."))
              world-data)))))
    world-data))


(defn civ-do-stuff [world-data civ-name {:keys [power-base priorities] :as civ}]
  (println "Do Stuff\n" civ)
  (let [base-actions (Math/ceil (/ power-base (inc 10)))
        max-actions (Math/ceil (/ power-base (inc 5)))
        curr-actions (rand-int-between base-actions max-actions)]
    ;if self.controlled_by_player:
    ;    print(f'YOU ARE CIV {self.name} ({self.symbol}).\n'
    ;   f'Power: {self.power:.2f} (Grow or expand to increase your power)\n'
    ;   f'Territory: {self.powerbase} (Expanding requires 10 power)\n'
    ;   f'Tech level: {self.techlevel:.2f} (Development requires 20 power)\n'
    ;   f'Instability: {self.instability:.2f} (Reducing instability is free)\n')
    (-> (reduce
          (fn [world action-n]
            (let [{:keys [power tech-level current-milestone] :as civ} (get-in world [:civs civ-name])
                  action (rand-nth priorities)]
              (println action-n action)
              (condp = action
                :territorial-expansion
                (do
                  (println (str civ-name " is trying to expand"))
                  (println power)
                  (if (> power 10)
                    (-> world
                      (update-in [:civs civ-name :power] - 10)
                      (civ-expand-territory civ))
                    (do (println (str civ-name " failed to expand"))
                      world)))

                :tech-development
                (do
                  (println (str civ-name " is developing"))
                  (if (> power 20)
                    (let [tech-level' (+ tech-level (/ (roll 10) 10))]
                      (-> world
                        (update-in [:civs civ-name :power] - 20)
                        (assoc-in [:civs civ-name :tech-level] tech-level')
                        ;; developing more advanced architecture
                        (cond->
                          (and
                            (> tech-level' (nth milestones current-milestone))
                            (< current-milestone (dec (count milestones))))
                          (->
                            (update-in [:civs civ-name :current-milestone] inc)
                            (update-in [:civs civ-name :architecture] conj (rand-nth shapes))))))
                    (do (println (str civ-name " failed to develop"))
                      world)))

                :population-growth
                (do
                  (println (str civ-name " is growing"))
                  (update-in world [:civs civ-name :bank] (fnil + 0) (* power (/ (roll 4) 10))))

                :territorial-stabilisation
                (do
                  (println (str civ-name " is stabilising"))
                  (update-in world [:civs civ-name :instability] * (rand)))

                :settlement-construction
                (do
                  (println (str civ-name " is building"))
                  (if (>= power 5)
                    (civ-build-settlement world civ)
                    (do (println (str civ-name " failed to build"))
                      world)))

                (do (println (str "Need to implement " action " action"))
                  world))))
          world-data
          (range curr-actions))
      ((fn [world]
         (let [bank (get-in world [:civs civ-name :bank] 0)]
           (-> world
             (update-in [:civs civ-name] dissoc :bank)
             (update-in [:civs civ-name :power] + bank))))))))

(defn settlement-tick [world-data {:keys [level x y] :as settlement}]
  ;; risk of settlements dying high initially, decreases over time
  (let [roll-fail? (< (roll 10) 3)
        fail? (< (reduce + (repeatedly 2 #(roll 100))) (- 100 level))
        ruin? (> level 5)]
    (cond-> world-data
      ;; if the level is high enough, keep a ruin
      (and roll-fail? fail? ruin?)
      (update :ruins conj settlement)

      (and roll-fail? fail?)
      (update :settlements dissoc [x y]))))

(defn civ-dissolve [world-data civ-name {:keys [age tech-level symbol territory power-base] :as civ}]
  (println (str civ-name " dissolved. It lasted " age " years and achieved a tech level of " tech-level))
  (let [world-data (-> world-data
                     (assoc-in [:civs civ-name :dead] true)
                     ;; TODO: Remove dead civ from civs?
                     ;; (update :civs dissoc civ-name)
                     (update :civ-letters str symbol))
        n (rand-int-between 1 (inc (Math/ceil (/ power-base 10))))
        remnants (take (min (count territory) n) (shuffle territory))]
    (if (seq territory)
      (-> (reduce
            (fn [{:keys [settlements] :as world} area]
              (cond-> world
                (get settlements area)
                (->
                  (update-in [:settlements area :history] conj :dissolved)
                  ;; weaken independent settlements and force collapse check
                  (update-in [:settlements area :level] - (rand-int-between 1 10))
                  (as-> $ (settlement-tick $ (get-in $ [:settlements area]))))))
            world-data
            territory)
        (update :civ-map #(into {} (remove (fn [[_loc name]] (= name civ-name))) %))
        (as-> $
          (reduce
            (fn [world-data [x y]]
              (spawn-civ world-data x y {:parent civ-name}))
            $
            remnants)))
      world-data)))

(defn civ-collapse [world-data civ-name {:keys [power-base territory] :as civ} {:keys [decimate?]}]
  (println (str civ-name " is collapsing."))
  (let [n (rand-int-between (Math/floor (/ (count territory) 1.5)) (count territory))
        lost-territory (take (min (count territory) n) (shuffle territory))]
    (-> world-data
      (update-in [:civs civ-name :instability] (reduce + (repeatedly (Math/ceil (/ power-base 10)) #(roll 6))))
      (update-in [:civs civ-name :power] (rand-int-between 2 4))
      (as-> $
        (reduce
          (fn [{:keys [settlements] :as world} area]
            (cond-> world
              (get settlements area)
              (->
                (update-in [:settlements area :history] conj :collapsed)
                ;; weaken independent settlements and force collapse check
                (update-in [:settlements area :level] - (rand-int-between 1 10))
                (as-> $ (settlement-tick $ (get-in $ [:settlements area]))))

              :always
              (update-in [:civs civ-name :territory] dissoc area)

              :always
              (update :civ-map dissoc area)))
          $
          lost-territory))
      (as-> $
        ;; natural disasters do not create new states
        (if (and decimate? (seq lost-territory))
          (let [n (rand-int-between 1 (inc (Math/ceil (/ power-base 10))))
                successor-states (take (min (count lost-territory) n) (shuffle lost-territory))]
            (-> (reduce
                  (fn [world-data [x y]]
                    (spawn-civ world-data x y {:parent civ-name :expand? true}))
                  $
                  successor-states))
            $)
          $)))))

(defn civ-tick [world-data civ-name {:keys [territory tech-level] :as civ}]
  (println "Tick\n" civ)
  (let [power-base' (count territory)]
    (-> world-data
      (update-in [:civs civ-name :age] inc)
      (cond->
        (not (seq territory))
        (as-> $ (civ-dissolve $ civ-name (get-in $ [:civs civ-name]))))
      ;; for each unit of land you hold you gain extra power
      (assoc-in [:civs civ-name :power-base] power-base')
      ;; national fortune = fortune (normal distribution) + tech
      (update-in [:civs civ-name :power] + (* power-base' (+ 0.8 (/ (- (reduce + (repeatedly 6 #(roll 10))) (reduce + (repeatedly 6 #(roll 10)))) 100))) (* power-base' tech-level 3))
      (update-in [:civs civ-name :instability] + (- (rand-int-between 1 (inc (Math/ceil (/ power-base' 10)))) (- (reduce + (repeatedly 3 #(roll 6))) (reduce + (repeatedly 3 #(roll 6))))))
      (as-> $
        (if (< (reduce + (repeatedly 2 #(roll 100))) (get-in $ [:civs civ-name :instability]))
          (if (= (roll 6) 1)
            (civ-dissolve $ civ-name (get-in $ [:civs civ-name]))
            (if (< (roll 6) 4)
              (civ-collapse $ civ-name (get-in $ [:civs civ-name]) {})
              (civ-collapse $ civ-name (get-in $ [:civs civ-name]) {:decimate? true})))
          $)))))

(comment
  (let [world-data (-> (generate-world (assoc (make-world "Apple" 2 2 {}) :sea-level -1)) (try-spawn-new-civs 10))
        civ-name   "Civ A+0"
        civ (get-in world-data [:civs civ-name])]
    (-> (civ-do-stuff world-data civ-name civ)
      (civ-tick civ-name civ)
      ((juxt :civ-map #(get-in % [:civs civ-name]) #_identity)))))

(defn world-tick [{:keys [civs] :as world-data}]
  (println :world-tick)
  (as-> world-data $
    (reduce-kv
      (fn [world loc {:keys [history level] :as _settlement}]
        (let [civ-name (last history)]
          (-> world
             ;; settlement has chance to grow
            (settlement-upgrade loc)
            ;; settlement gives power boost to it's owner
            (update-in [:civs civ-name :power] + (* level (roll 10)))
            ;; settlements have a chance to collapse
            (as-> $ (settlement-tick $ (get-in $ [:settlements loc]))))))
      $
      (get $ :settlements))
    (reduce-kv
      (fn [world civ-name civ]
        (-> world
          (civ-do-stuff civ-name civ)
          (civ-tick civ-name civ))
        world)
      $
      civs)))

(defn on-tick [world-data]
  (cond-> (world-tick world-data)
    (= (roll 20) 20)
    (try-spawn-new-civs (roll 6))))

(comment
  (:temp-noise (make-world "Apple" 5 6 {}))

  (-> (generate-world (assoc (make-world "Apple" 2 2 {}) :sea-level -1))
    (try-spawn-new-civs 10)
    on-tick)

  ,)

(comment
  (reduce
    (fn [[v stack dice] chr]
      (cond
        (= \d chr) [v [(Integer/parseInt (str/join stack))] []]
        (Character/isDigit chr) [v (conj stack chr) dice]

        :else [(conj v chr) stack dice]))
    [[] [] []]
    "2d6" #_"12d6+1exp[6,7,8,9,10]"))

