(ns fruit-economy.sim.basic
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [datascript.core :as d]
            [fruit-economy.state :as state]
            [fruit-economy.gen-land :refer [make-temp-noise-map make-elev-noise-map process-noise-map]]
            [fruit-economy.data.core :refer [lookup-avet]]
            [fruit-economy.sim.market :refer [empty-order-book load-order load-orders remove-order match-orders market-summary]]
            [clojure.string :as str]
            [clj-async-profiler.core :as prof]
            [taoensso.timbre :refer [log]]
            [taoensso.tufte :as tufte]
            [fruit-economy.infer.core :refer [rewrite infer infer-conn]]
            [fruit-economy.utils :refer [weighted-rand]])
  (:import [io.github.humbleui.skija Paint]))


(defn coord+entity->entities-coll [coord+entity]
  (reduce
    (fn [m [coord entity]]
      (conj m (assoc entity :place [:coord coord])))
    []
    coord+entity))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4)) :max-age (+ (rand-int 5) 20)})

(defn resource-fn [local-elev sea-level x] (if (>= local-elev sea-level) (int (* (/ (+ x 1) 2) 5)) 0))

(defn decide-biome [local-temp local-elev sea-level]
  (cond
    (and (>= local-elev sea-level) (< local-elev 0.1)) :beach
    (and (>= local-elev 0.4) (< local-temp -0.2)) :snow-mountain
    (>= local-elev 0.4) :mountain
    ;; temperate region
    (> local-elev sea-level)
    (cond
      (< local-temp -0.2) :snow
      (< local-temp 0) :tundra
      (< local-temp 0.1) :grassland
      (< local-temp 0.2) :forest
      (< local-temp 0.3) :jungle
      :else :desert)
    :else :ocean))

(defn gen-bug-world [size n-peeps]
  (let [temp-noise (make-temp-noise-map size size)
        elev-noise (make-elev-noise-map size size)
        temp-mod 0.1 elev-mod 0
        temp (process-noise-map temp-noise temp-mod)
        elev (process-noise-map elev-noise elev-mod)
        sea-level (rand-nth (range 0.001 0.009 0.001))]
    (into
      [{:game/started? false
        :game/score 0
        :game/start-time 0
        :day 0}
       {:action/start (System/currentTimeMillis)}]
      (concat
        (for [x (range size)
              y (range size)]
          (let [local-temp (get-in temp [y x]) local-elev (get-in elev [y x])
                food (resource-fn local-elev sea-level local-temp)
                rock (resource-fn local-elev sea-level local-elev)]
            {:init-food food :food food :rock rock :coord [x y] :temp local-temp :elev local-elev :biome (decide-biome local-temp local-elev sea-level)}))
        (coord+entity->entities-coll
          (repeatedly n-peeps (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))))))


(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(defn reset-world-db []
  (d/db-with (d/empty-db {:day {:db/index true}
                          :money {:db/index true}
                          :kind {:db/index true}
                          :good {:db/index true}
                          :place {:db/valueType :db.type/ref}
                          :coord {:db/unique :db.unique/identity}
                          :settlement/place {:db/valueType :db.type/ref}
                          :hometown {:db/valueType :db.type/ref}
                          :governs {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/one}})
    (gen-bug-world bug-world-size bug-count)))

(defn reset-world []
  (let [db (reset-world-db)]
    {:world-db db :world-conn (d/conn-from-db db) :map-view :default-view :day 0}))

(defn apply-rules [world-conn decision-rules reaction-rules]
  (-> world-conn
    (infer-conn decision-rules 1)
    (infer-conn reaction-rules 1)))

(defn lookup-day [db]
  (:day (first (lookup-avet db :day nil))))

(defn gen-summary [db]
  (let [peeps (lookup-avet db :kind :peep)
        food-factories (lookup-avet db :kind :food-factory)
        clothes-factories (lookup-avet db :kind :clothes-factory)
        cities (lookup-avet db :kind :city)
        money (lookup-avet db :money nil)
        day (lookup-day db)]
    {:peeps peeps
     :food-factories food-factories
     :clothes-factories clothes-factories
     :cities cities
     :money money
     :day day}))

(defn print-summary [db]
  (let [tableise (fn [m k]
                   (let [[k v] (if (vector? k) [(first k) (get m (second k) (second k))] [k (get m k k)])]
                     (format (str "%" (count (str k)) "s") v)))

        {:keys [peeps food-factories clothes-factories cities money day]} (gen-summary db)
        peep-keys (into [] (comp (map keys) cat (distinct)) peeps)
        food-factory-keys (into [] (comp (map keys) cat (distinct)) food-factories)
        clothes-factory-keys (into [] (comp (map keys) cat (distinct)) clothes-factories)
        city-keys (into [] (comp (map keys) cat (distinct)) cities)
        market-keys [[:market :price :price-float :price-velocity :supply :demand]
                     [:food :food/price :food/price-float :food/price-velocity :food/last-supply :food/last-demand]
                     [:cloth :clothes/price :clothes/price-float :clothes/price-velocity :clothes/last-supply :clothes/last-demand]
                     [:labour :labour/price :labour/price-float :labour/price-velocity :labour/last-supply :labour/last-demand]]

        tick (:day (first day))

        show-peep-summary false
        show-food-factory-summary false
        show-clothes-factory-summary false
        show-city-summary true
        show-city-detail false
        show-stats-summary true]
    (println (str "--- BEGIN TURN " tick " ---"))
    (when show-peep-summary
      (println "  |--- BEGIN PEEPS ---")
      (println peep-keys)
      (doseq [peep peeps]
        (println (mapv (partial tableise peep) peep-keys)))
      (println "  |--- END PEEPS ---"))
    (when show-food-factory-summary
      (println "  |--- BEGIN FOOD FACTORIES ---")
      (println food-factory-keys)
      (doseq [food-factory food-factories]
        (println (mapv (partial tableise food-factory) food-factory-keys)))
      (println "  |--- END FOOD FACTORIES ---"))
    (when show-clothes-factory-summary
      (println "  |--- BEGIN CLOTHES FACTORIES ---")
      (println clothes-factory-keys)
      (doseq [clothes-factory clothes-factories]
        (println (mapv (partial tableise clothes-factory) clothes-factory-keys)))
      (println "  |--- END CLOTHES FACTORIES ---"))
    (when show-city-summary
      (println "  |--- BEGIN CITIES ---")
      (when show-city-detail
        (println city-keys)
        (doseq [city cities]
          (println (mapv (partial tableise city) city-keys))))
      (println (select-keys (first cities) [:clothes/last-supply]))
      (doseq [market market-keys
              :let [city (first cities)
                    mks (first market-keys)]]
        (println (mapv (partial tableise city) (map vector mks market))))
      (println "  |--- END CITIES ---"))
    (when show-stats-summary
      (println "  |--- BEGIN STATS ---")
      (println "Day:" tick)
      (println "Peeps:" (count peeps))
      (println "Food Factories:" (count food-factories))
      (println "Clothes Factories:" (count clothes-factories))
      (println "Money Supply:" (reduce + (mapv :money money)))
      (println "  |--- END STATS ---"))
    (println (str "--- END TURN " tick " ---"))))

(defn add-stats [stats world-db]
  (let [{:keys [peeps] :as summary} (gen-summary world-db)]
    (if (seq peeps)
      (conj stats summary)
      stats)))

(defn track-db [dbs world-db]
  (let [cities (lookup-avet world-db :kind :city)]
    (if (seq cities)
      (conj dbs world-db)
      dbs)))

(when (nil? @state/*world)
  (reset! state/*world (reset-world)))

(defn touch [e]
  (if e
    (d/touch e)
    e))

(defn doc-q [db]
  (d/q
    '[:find (pull ?e [*])
      :where
      [?e :db/doc _]]
    db))

(comment
  (doc-q (:world-db @state/*world)))

(defn coord-q [db coord]
  (-> (lookup-avet db :coord coord)
    first
    touch))

(defn units-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :place)
    (mapv :db/id)
    (d/pull-many db '[* {:place [:coord]}])
    (mapv #(update % :place :coord))))

(defn settlements-q
  "coord can be nil or [x y]"
  [db coord]
  (->> (when coord
         [:coord coord])
    (lookup-avet db :settlement/place)
    (mapv :db/id)
    (d/pull-many db '[* {:settlement/place [:coord] :_governs [*]}])
    (mapv #(update % :settlement/place :coord))))

(def start-rule
  {:when '[[_ :action/start ?now ?max-tx]
           [(max-tx $) ?max-tx]
           [?game :game/started? false]]
   :then '[[:db/add ?game :game/started? true]
           [:db/add ?game :game/start-time ?now]
           [:db/add :db/current-tx :db/doc "Start"]]
   :args {'max-tx :max-tx}})

(def days-pass-rule
  {:when '[[?e :day ?day]
           [(inc ?day) ?next-day]]
   :then '[[:db/add ?e :day ?next-day]]})

(def time-tick-rule
  '{:when [[_ :action/update-time ?now ?max-tx]
           [(max-tx $) ?max-tx]
           [?game :game/started?]
           [?flappy :flappy/start-time ?flappy-start-time]
           [(- ?now ?flappy-start-time) ?time-delta]]
    :then [[:db/add ?game :game/current-time ?now]
           [:db/add ?game :game/time-delta ?time-delta]
           [:db/add :db/current-tx :db/doc "Time update"]]
    :args {'max-tx :max-tx}})

(defn sees [coord vision]
  (let [[x y] coord]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] coord)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] coord)]
         [x y])])))

(defn best-food [db coord sees]
  (let [get-food (fn [coord]
                   (-> (lookup-avet db :coord coord [:food])
                     (first)
                     (get :food 0)))]
    (reduce
      (fn [[coord food] target]
        (let [target-food (get-food target)]
          (if (> target-food food)
            [target target-food]
            [coord food])
          (if (> target-food food)
            [target target-food]
            [coord food])))
      [coord (get-food coord)]
      sees)))

(defn hunt [db coord vision]
  (let [[best-food-coord _food] (best-food db coord (sees coord vision))]
    best-food-coord))

(def hunt-rule
  {:when '[[?e :place ?ce]
           [?e :vision ?vision]
           [?ce :coord ?coord]
           [?ce :food ?food]
           [(hunt $ ?coord ?vision) ?target]
           [(not= ?target ?coord)]
           [?te :coord ?target]]
   :then '[[:db/add ?e :place ?te]]
   :args {'hunt hunt}})

(defn gathered [food] (min food (+ (quot food 2) 2)))

(def try-eat-rule
  {:when '[[?e :place ?ce]
           [?e :wealth ?wealth]
           [?e :hunger ?hunger]
           [?ce :coord ?coord]
           [?ce :food ?food]
           [(gathered ?food) ?gather]
           [(- ?food ?gather) ?rem-food]
           [(+ ?wealth ?gather) ?gain-wealth]
           [(- ?gain-wealth ?hunger) ?rem-wealth]]
   :then '[[:db/add ?e :wealth ?rem-wealth]
           [:db/add ?ce :food ?rem-food]]
   :args {'gathered gathered}})

(def remove-starving-rule
  '{:when [[?e :wealth ?wealth]
           [(< ?wealth 0)]]
    :then [[:db/retractEntity ?e]]})

(defn grow-food [food init-food] (min (inc food) init-food))

(def grow-food-rule
  {:when '[[?e :food ?food]
           [?e :init-food ?init-food]
           [(< ?food ?init-food)]
           [(grow-food ?food ?init-food) ?gain-food]]
   :then '[[:db/add ?e :food ?gain-food]]
   :args {'grow-food grow-food}})

(defn spawn-factory-tx [idx attrs]
  (let [base-planning (rand-nth [1 3 5 8 15])]
    (into [[:db/add idx :hometown -1]
           [:db/add idx :money 10000]
           [:db/add idx :owed-tax 0]
           [:db/add idx :inventory 0]
           [:db/add idx :sold 0]
           [:db/add idx :last-sold 0]
           [:db/add idx :earned 0]
           [:db/add idx :last-earned 0]
           [:db/add idx :min-labour 2]
           [:db/add idx :labour-bought 0]
           [:db/add idx :base-planning base-planning]
           [:db/add idx :planning base-planning]
           [:db/add idx :labour/consumed 0]
           [:db/add idx :labour/last-consumed 0]]
      (map (fn [[k v]] [:db/add idx k v]))
      attrs)))

(def create-settlement-rule
  {:when '[[?e :wealth ?wealth]
           [(get-else $ ?e :settlement :db/missing) ?s]
           [(= :db/missing ?s)]
           [(>= ?wealth 10)]
           [(- ?wealth 10) ?rem-wealth]
           [?e :place ?place]
           [(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ?letter]
           [(str ?letter) ?name]]
   :then (into
           '[[:db/add ?e :wealth ?rem-wealth]
             [:db/add ?e :settlement -1]
             [:db/add -1 :kind :city]
             [:db/add -1 :settlement/name ?name]
             [:db/add -1 :settlement/place ?place]
             [:db/add -1 :food/market ?empty-order-book]
             [:db/add -1 :clothes/market ?empty-order-book]
             [:db/add -1 :labour/market ?empty-order-book]
             [:db/add -1 :food/market-price 0]
             [:db/add -1 :clothes/market-price 0]
             [:db/add -1 :labour/market-price 0]
             [:db/add -1 :food/price 1]
             [:db/add -1 :food/price-float 1]
             [:db/add -1 :food/price-velocity 1]
             [:db/add -1 :food/price-history []]
             [:db/add -1 :food/demand 1]
             [:db/add -1 :food/supply 1]
             [:db/add -1 :food/last-demand 1]
             [:db/add -1 :food/last-supply 1]
             [:db/add -1 :food/produced 0]
             [:db/add -1 :food/consumed 0]
             [:db/add -1 :food/last-produced 0]
             [:db/add -1 :food/last-consumed 0]
             [:db/add -1 :clothes/price 1]
             [:db/add -1 :clothes/price-float 1]
             [:db/add -1 :clothes/price-velocity 1]
             [:db/add -1 :clothes/price-history []]
             [:db/add -1 :clothes/demand 1]
             [:db/add -1 :clothes/supply 1]
             [:db/add -1 :clothes/last-demand 1]
             [:db/add -1 :clothes/last-supply 1]
             [:db/add -1 :clothes/produced 0]
             [:db/add -1 :clothes/consumed 0]
             [:db/add -1 :clothes/last-produced 0]
             [:db/add -1 :clothes/last-consumed 0]
             [:db/add -1 :labour/price 1]
             [:db/add -1 :labour/price-float 1]
             [:db/add -1 :labour/price-velocity 1]
             [:db/add -1 :labour/price-history []]
             [:db/add -1 :labour/demand 1]
             [:db/add -1 :labour/supply 1]
             [:db/add -1 :labour/last-demand 1]
             [:db/add -1 :labour/last-supply 1]
             [:db/add -1 :labour/produced 0]
             [:db/add -1 :labour/consumed 0]
             [:db/add -1 :labour/last-produced 0]
             [:db/add -1 :labour/last-consumed 0]
             [:db/add -2 :tax-rate 10]
             [:db/add -2 :money 10000]
             [:db/add -2 :sold 0]
             [:db/add -2 :earned 0]
             [:db/add -2 :food-stockpile 0]
             [:db/add -2 :clothes-stockpile 0]
             [:db/add -2 :infrastructure/size 1]
             [:db/add -2 :infrastructure/hp 100]
             [:db/add -2 :infrastructure/maintenance 1000]
             [:db/add -2 :infrastructure/labour-bought 0]
             [:db/add -2 :infrastructure/repair 0]
             [:db/add -2 :governs -1]]
           (comp cat cat)
           [;; peeps
            (for [idx (range 10)]
              [[:db/add (- idx 100) :kind :peep]
               [:db/add (- idx 100) :hometown -1]
               [:db/add (- idx 100) :money 10000]
               [:db/add (- idx 100) :owed-tax 0]
               [:db/add (- idx 100) :labour 10]
               [:db/add (- idx 100) :health 10]
               [:db/add (- idx 100) :food 10]
               [:db/add (- idx 100) :clothes 10]
               [:db/add (- idx 100) :min-food 2]
               [:db/add (- idx 100) :min-clothes 1]
               [:db/add (- idx 100) :planning (rand-nth [1 3 5 8 15])]
               [:db/add (- idx 100) :belief/food-price 1]
               [:db/add (- idx 100) :belief/clothes-price 1]
               [:db/add (- idx 100) :belief/labour-price 40]
               [:db/add (- idx 100) :sold 0]
               [:db/add (- idx 100) :last-sold 0]
               [:db/add (- idx 100) :earned 0]
               [:db/add (- idx 100) :last-earned 0]
               [:db/add (- idx 100) :labour/produced 0]
               [:db/add (- idx 100) :food/consumed 0]
               [:db/add (- idx 100) :clothes/consumed 0]
               [:db/add (- idx 100) :labour/last-produced 0]
               [:db/add (- idx 100) :food/last-consumed 0]
               [:db/add (- idx 100) :clothes/last-consumed 0]])
            ;; food factories
            (for [idx (range 5)]
              (spawn-factory-tx (- idx 200) {:kind :food-factory :good :food :decay 0.98 :production 0.7
                                             :food/produced 0 :food/last-produced 0
                                             :belief/food-price 1 :belief/labour-price 40}))
            ;; clothes factories
            (for [idx (range 5)]
              (spawn-factory-tx (- idx 300) {:kind :clothes-factory :good :clothes :decay 0.9 :production 2
                                             :clothes/produced 0 :clothes/last-produced 0
                                             :belief/clothes-price 1 :belief/labour-price 40}))
            ;; farmer
            (for [idx (range 10)]
              [[:db/add (- idx 500) :kind :farmer]
               [:db/add (- idx 500) :hometown -1]
               [:db/add (- idx 500) :money 10000]
               [:db/add (- idx 500) :farm-size 1]
               [:db/add (- idx 500) :decay 0.9]
               [:db/add (- idx 500) :good :food]
               [:db/add (- idx 500) :owed-tax 0]
               [:db/add (- idx 500) :inventory 1000]
               [:db/add (- idx 500) :planning (rand-nth [1 3 5 8 15])]
               [:db/add (- idx 500) :belief/food-price 10]
               [:db/add (- idx 500) :sold 0]
               [:db/add (- idx 500) :last-sold 0]
               [:db/add (- idx 500) :earned 0]
               [:db/add (- idx 500) :last-earned 0]
               [:db/add (- idx 500) :food/produced 0]
               [:db/add (- idx 500) :food/last-produced 0]])])
   :args {'rand-nth rand-nth
          '?empty-order-book (empty-order-book)}})

(defn like-to-buy [money price percentage]
  (let [budget (int (* money percentage))]
    (quot budget price)))

(defn gen-orders [quantity-wanted price factories]
  (->
    (reduce
      (fn [[orders to-buy] {:keys [inventory labour] :as from}]
        (let [buyable (or inventory labour)]
          (cond
            (< buyable 1) [orders to-buy]
            (> to-buy 0)
            (let [buying (min to-buy buyable)
                  rem (- to-buy buying)
                  orders' (conj orders {:from from :buy buying :price price})]
              (if (<= rem 0)
                (reduced [orders' 0])
                [orders' rem]))
            :else [orders to-buy])))
      [[] quantity-wanted]
      factories)
    first))

(defn value-size-penalise-price
  "m is a {:price :size} map"
  [m]
  (reduce-kv
    (fn [m price size]
      (assoc m price (/ size price)))
    {}
    m))

(def peep-shop-rule
  (let [shop
        (fn [db peep-eid]
          (let [{:keys [money planning min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                food-plan (* min-food planning) clothes-plan (* min-clothes planning)
                {home-eid :db/id
                 food-price :food/price
                 clothes-price :clothes/price
                 food-market :food/market
                 clothes-market :clothes/market
                 :as home} (get peep :hometown)

                existing-food-order? (get-in food-market [:buys peep-eid])
                food-price' (if existing-food-order?
                              (let [inc-food-price (inc food-price)
                                    market-sell (get-in food-market [:summary :size :sell] {inc-food-price 10})
                                    weighted-values (value-size-penalise-price market-sell)
                                    rand-price (weighted-rand weighted-values)
                                    min-price (get-in food-market [:summary :stat :sell :min] inc-food-price)]
                                (rand-nth [min-price rand-price inc-food-price]))
                              (rand-nth [food-price (max (dec food-price) 1)]))

                existing-clothes-order? (get-in clothes-market [:buys peep-eid])
                clothes-price' (if existing-clothes-order?
                                 (inc clothes-price)
                                 (rand-nth [clothes-price (max (dec clothes-price) 1)]))

                food-like (like-to-buy money food-price' 0.4)
                clothes-like (like-to-buy money clothes-price' 0.2)
                food-want (min food-like food-plan) clothes-want (min clothes-like clothes-plan)]
            (log :debug peep-eid :shop food-want clothes-want (d/touch peep))
            (log :debug :food-like food-like :food-plan food-plan :clothes-like clothes-like :clothes-plan clothes-plan :food/demand (:food/demand home) :clothes/demand (:clothes/demand home))
            (log :trace (mapv d/touch (lookup-avet db :good nil)))
            (cond-> []
              (> food-want 0)
              (conj [:db/add home-eid :food/market (load-order food-market {:price food-price' :size food-want :side :buys :id peep-eid :good-kw :food})])
              (> clothes-want 0)
              (conj [:db/add home-eid :clothes/market (load-order clothes-market {:price clothes-price' :size clothes-want :side :buys :id peep-eid :good-kw :clothes})]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :planning ?planning]
             [?e :food ?food]
             [?e :clothes ?clothes]
             [?e :min-food ?min-food]
             [?e :min-clothes ?min-clothes]
             [(* ?min-food ?planning) ?food-plan]
             [(* ?min-clothes ?planning) ?clothes-plan]
             [(<= ?food ?food-plan) ?not-enough-food]
             [(<= ?clothes ?clothes-plan) ?not-enough-clothes]
             [(or ?not-enough-food ?not-enough-clothes)]
             [?home :food/price ?food-price]
             [?home :clothes/price ?clothes-price]]
     :then '[[:db.fn/call shop ?e]]
     :call {'shop shop}}))

(def peep-consume-rule
  (let [consume (fn [db peep-eid]
                  (let [{:keys [min-food min-clothes food clothes belief/labour-price] :as peep} (d/entity db peep-eid)
                        base-labour 10 ;; reset labour

                        food-rem (max (- food min-food) 0)
                        clothes-rem (max (- clothes min-clothes) 0)

                        food-had (- food food-rem)
                        clothes-had (- clothes clothes-rem)

                        enough-food? (>= food-had min-food)
                        enough-clothes? (>= clothes-had min-clothes)
                        sold (:last-sold peep)
                        hometown (:hometown peep)
                        hometown-eid (:db/id hometown)
                        labour-market (:labour/market hometown)
                        ;;labour-price (:labour/price hometown)
                        labour-produced (:labour/produced hometown)
                        food-consumed (:food/consumed hometown)
                        clothes-consumed (:clothes/consumed hometown)
                        food-price (:food/price hometown)
                        clothes-price (:clothes/price hometown)

                        planning (:planning peep)
                        peep-food-consumed (:food/last-consumed peep)
                        peep-clothes-consumed (:clothes/last-consumed peep)

                        min-labour-value (+ (* planning peep-food-consumed food-price) (* planning peep-clothes-consumed clothes-price))

                        existing-order? (get-in labour-market [:sell peep-eid])
                        no-work? (zero? sold)
                        fully-working? (and (not existing-order?) (> sold 0))
                        labour-price' (cond
                                        no-work?
                                        (max (dec labour-price) 1 min-labour-value)

                                        fully-working?
                                        (inc labour-price)

                                        :else labour-price)]
                    (cond-> [[:db/add peep-eid :food food-rem]
                             [:db/add peep-eid :clothes clothes-rem]
                             [:db/add peep-eid :labour base-labour]
                             [:db/add peep-eid :labour/produced (+ (:labour/produced peep) base-labour)]
                             [:db/add peep-eid :food/consumed (+ (:food/consumed peep) food-had)]
                             [:db/add peep-eid :clothes/consumed (+ (:clothes/consumed peep) clothes-had)]
                             [:db/add hometown-eid :labour/market (load-order labour-market {:price labour-price' :size base-labour :side :sell :id peep-eid :good-kw :labour})]
                             [:db/add hometown-eid :labour/produced (+ labour-produced base-labour)]
                             [:db/add hometown-eid :food/consumed (+ food-consumed food-had)]
                             [:db/add hometown-eid :clothes/consumed (+ clothes-consumed clothes-had)]]
                      (or (not enough-food?) (not enough-clothes?))
                      (conj [:db/add peep-eid :health (max (dec (:health peep)) 0)])

                      (and enough-food? enough-clothes?)
                      (conj [:db/add peep-eid :health (min (inc (:health peep)) 10)])

                      (not= labour-price labour-price')
                      (conj [:db/add peep-eid :belief/labour-price labour-price'])

                      #_#_;; let's not have labour ability be dynamic, we can worry about that later...
                      (or enough-food? enough-clothes?)
                      (conj [:db/add (:db/id hometown) :labour/supply (+ labour-supply (if enough-food? 5 2) (if enough-clothes? 5 2))]))))]
    {:when '[[?e :food _]
             [?e :clothes _]]
     :then '[[:db.fn/call consume ?e]]
     :call {'consume consume}}))

(let [world (:world-db @state/*world)]
  (infer world [peep-consume-rule] 1)
  nil)

(def adjust-factories-planning-rule
  (let [adjust-factory (fn [_db factory-eid sold wanted planning base-planning]
                         (if (> sold wanted)
                           [[:db/add factory-eid :planning (inc planning)]]
                           [[:db/add factory-eid :planning (max (dec planning) base-planning)]]))]
    {:when '[[?e :last-sold ?sold]
             [?e :inventory ?inventory]
             [?e :planning ?planning]
             [?e :base-planning ?base-planning]
             [(* ?planning 10) ?wanted]
             [(not= ?sold ?wanted)]]
     :then '[[:db.fn/call adjust-factory ?e ?sold ?wanted ?planning ?base-planning]]
     :call {'adjust-factory adjust-factory}}))

(def reset-entity-rule
  (let [entity-sold (fn [db eid]
                      (let [{food-supply :food/supply
                             food-demand :food/demand
                             food-produced :food/produced
                             food-consumed :food/consumed
                             clothes-supply :clothes/supply
                             clothes-demand :clothes/demand
                             clothes-produced :clothes/produced
                             clothes-consumed :clothes/consumed
                             labour-supply :labour/supply
                             labour-demand :labour/demand
                             labour-produced :labour/produced
                             labour-consumed :labour/consumed
                             :keys [sold]} (d/entity db eid)]
                        ;;(println :reset-entity-rule :e (d/touch (d/entity db eid)))
                        (println :reset-entity-rule :eid eid :kind (:kind (d/entity db eid)) :sold sold
                          :food-supply food-supply :food-demand food-demand :food-produced food-produced :food-consumed food-consumed
                          :clothes-supply clothes-supply :clothes-demand clothes-demand :clothes-produced clothes-produced :clothes-consumed clothes-consumed
                          :labour-supply labour-supply :labour-demand labour-demand :labour-produced labour-produced :labour-consumed labour-consumed)
                        (cond-> []
                          sold
                          (into [[:db/add eid :sold 0]
                                 [:db/add eid :last-sold sold]])
                          food-supply
                          (into [[:db/add eid :food/supply 0]
                                 [:db/add eid :food/last-supply food-supply]])
                          food-demand
                          (into [[:db/add eid :food/demand 0]
                                 [:db/add eid :food/last-demand food-demand]])
                          food-produced
                          (into [[:db/add eid :food/produced 0]
                                 [:db/add eid :food/last-produced food-produced]])
                          food-consumed
                          (into [[:db/add eid :food/consumed 0]
                                 [:db/add eid :food/last-consumed food-consumed]])
                          clothes-supply
                          (into [[:db/add eid :clothes/supply 0]
                                 [:db/add eid :clothes/last-supply clothes-supply]])
                          clothes-demand
                          (into [[:db/add eid :clothes/demand 0]
                                 [:db/add eid :clothes/last-demand clothes-demand]])
                          clothes-produced
                          (into [[:db/add eid :clothes/produced 0]
                                 [:db/add eid :clothes/last-produced clothes-produced]])
                          clothes-consumed
                          (into [[:db/add eid :clothes/consumed 0]
                                 [:db/add eid :clothes/last-consumed clothes-consumed]])
                          labour-supply
                          (into [[:db/add eid :labour/supply 0]
                                 [:db/add eid :labour/last-supply labour-supply]])
                          labour-demand
                          (into [[:db/add eid :labour/demand 0]
                                 [:db/add eid :labour/last-demand labour-demand]])
                          labour-produced
                          (into [[:db/add eid :labour/produced 0]
                                 [:db/add eid :labour/last-produced labour-produced]])
                          labour-consumed
                          (into [[:db/add eid :labour/consumed 0]
                                 [:db/add eid :labour/last-consumed labour-consumed]]))))]
    {:when '[(or
               [?e :sold _]
               [?e :food/produced _]
               [?e :food/consumed _]
               [?e :food/supply _]
               [?e :food/demand _]
               [?e :clothes/produced _]
               [?e :clothes/consumed _]
               [?e :clothes/supply _]
               [?e :clothes/demand _]
               [?e :labour/produced _]
               [?e :labour/consumed _]
               [?e :labour/supply _]
               [?e :labour/demand _])]
     :then '[[:db.fn/call entity-sold ?e]]
     :call {'entity-sold entity-sold}}))

(def hire-rule
  (let [hire (fn [db factory-eid]
               (log :info :hire)
               (let [{:keys [money last-sold planning min-labour belief/labour-price labour-bought inventory decay production good] :as factory} (d/entity db factory-eid)
                     _ (log :debug :sold last-sold)
                     labour-plan (* min-labour planning 10)
                     {home-eid :db/id
                      ;;labour-price :labour/price
                      labour-market :labour/market
                      :as home} (get factory :hometown)

                     existing-order? (get-in labour-market [:buys factory-eid])
                     no-hires? (zero? labour-bought)
                     fully-hired? (and (not existing-order?) (> labour-bought 0))
                     labour-price' (cond
                                     fully-hired?
                                     (max (dec labour-price) 1)

                                     no-hires?
                                     (let [inc-labour-price (inc labour-price)
                                           market-sell (get-in labour-market [:summary :size :sell] {inc-labour-price 1})
                                           weighted-values (value-size-penalise-price market-sell)
                                           rand-price (weighted-rand weighted-values)
                                           min-price (get-in labour-market [:summary :stat :sell :min] inc-labour-price)]
                                       (rand-nth [min-price rand-price inc-labour-price]))

                                     :else labour-price)
                     _ (log :debug :hire :labour-market-summary (:summary labour-market))

                     labour-like (like-to-buy money labour-price' 0.8)
                     labour-want (min labour-like labour-plan)
                     _ (log :debug :hire :labour-like labour-like :money money :labour-want labour-want)]
                 (cond-> (when (> labour-want 0)
                           [[:db/add home-eid :labour/market (load-order labour-market {:price labour-price' :size labour-want :side :buys :id factory-eid :good-kw :labour-bought})]])
                   (not= labour-price labour-price')
                   (conj [:db/add factory-eid :belief/labour-price labour-price']))))]
    ;; This models ad-hoc labour, finding work, which could be dice roll based etc, is separate to this, peeps keep doing ad-hoc work while "looking" for a job until they find one, then they stop, unless they switch again.
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :inventory ?inventory]
             ;;[(< ?inventory 100)]
             [?e :kind ?kind]
             [(contains? #{:food-factory :clothes-factory} ?kind)]]
     :then '[[:db.fn/call hire ?e]]
     :call {'hire hire}}))

(def craft-rule
  (let [craft (fn [db factory-eid]
                (let [{:keys [money last-sold min-labour labour-bought inventory decay production good] :as factory} (d/entity db factory-eid)
                      _ (log :debug :sold last-sold)
                      {home-eid :db/id
                       labour-consumed :labour/consumed
                       :as home} (get factory :hometown)
                      ;; Dig into negative money?
                      _ (log :debug :craft :money money :production production :labour-bought labour-bought)
                      ;; TODO: Model pricing to also take into account cost of inputs (bill of materials + labour) as well as willingness to accept discount under extreme conditions.
                      produced (condp = good :food (long (* production labour-bought)) :clothes (long (* production (Math/log (+ labour-bought 1)))))
                      decayed-inventory (long (* inventory decay))
                      inventory' (+ decayed-inventory produced)
                      good-market-key (condp = good :food :food/market :clothes :clothes/market)
                      good-price-key (condp = good :food :food/price :clothes :clothes/price)
                      good-produced-key (condp = good :food :food/produced :clothes :clothes/produced)

                      market (good-market-key home)
                      existing-order? (get-in market [:sell factory-eid])

                      price (good-price-key home)
                      sold-nothing? (zero? last-sold)
                      sold-out? (and (not existing-order?) (> last-sold 0))
                      price' (cond
                               sold-nothing?
                               (max (dec price) 1)

                               sold-out?
                               (inc price)

                               :else price)]
                  (log :debug :sold last-sold :existing-order? existing-order?)
                  (log :debug factory-eid :craft :labour-bought labour-bought :produced produced :good good good-produced-key (good-produced-key home) (d/touch factory))
                  (cond-> [[:db/add factory-eid :inventory inventory']]

                    (> inventory' 0)
                    (conj [:db/add home-eid good-market-key (load-order market {:price price' :size inventory' :side :sell :id factory-eid :good-kw :inventory})])

                    (and (zero? inventory') existing-order?)
                    (conj [:db/add home-eid good-market-key (remove-order market :sell factory-eid)])

                    (>= labour-bought min-labour)
                    (into
                      [[:db/add home-eid :labour/consumed (+ labour-consumed labour-bought)]
                       [:db/add home-eid good-produced-key (+ (good-produced-key home) produced)]
                       [:db/add factory-eid :labour/consumed (+ (:labour/consumed factory) labour-bought)]
                       [:db/add factory-eid good-produced-key (+ (good-produced-key factory) produced)]]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :inventory ?inventory]
             ;;[(< ?inventory 100)]
             [?e :kind ?kind]
             [(contains? #{:food-factory :clothes-factory} ?kind)]]
     :then '[[:db.fn/call craft ?e]]
     :call {'craft craft}}))


(def farmers-farm-rule
  (let [farm (fn [db farmer-eid day]
               (let [{:keys [money last-sold inventory decay good farm-size] :as factory} (d/entity db farmer-eid)
                     _ (log :debug :sold last-sold)
                     {home-eid :db/id
                      :as home} (get factory :hometown)
                     ;; Dig into negative money?
                     _ (log :debug :farmers-farm :money money)
                     ;; first of october, get harvest
                     produced (if (= 300 (mod day 360)) (* 1000 farm-size) 0)
                     decayed-inventory (long (* inventory decay))
                     inventory' (+ decayed-inventory produced)
                     _ (log :debug :farmers-farm :day (mod day 360) :farm-size farm-size :inventory inventory :produced produced :decayed-inventory decayed-inventory :inventory' inventory')
                     good-market-key :food/market
                     good-price-key :food/price
                     good-produced-key :food/produced

                     market (good-market-key home)
                     existing-order? (get-in market [:sell farmer-eid])

                     ;; Farmers have a stock, which is their "personal" supply, they will start selling from that stock if the price is high enough
                     ;; They will treat the price of food as the sum of all wheat available.

                     ;; Player action - Place a farm
                     ;; Make all prices based on belief! Try this in asami?
                     ;; Just have price belief + order books? How minimal can we make it?

                     price (good-price-key home)
                     sold-nothing? (zero? last-sold)
                     sold-out? (and (not existing-order?) (> last-sold 0))
                     price' (cond
                              sold-nothing?
                              (max (dec price) 10)

                              sold-out?
                              (inc price)

                              :else price)]
                 (log :debug :sold last-sold :existing-order? existing-order?)
                 (log :debug farmer-eid :farmers-farm :produced produced :good good good-produced-key (good-produced-key home) (d/touch factory))
                 (cond-> [[:db/add farmer-eid :inventory inventory']]

                   (> inventory' 0)
                   (conj [:db/add home-eid good-market-key (load-order market {:price price' :size inventory' :side :sell :id farmer-eid :good-kw :inventory})])

                   (and (zero? inventory') existing-order?)
                   (conj [:db/add home-eid good-market-key (remove-order market :sell farmer-eid)])

                   (> produced 0)
                   (into
                     [[:db/add home-eid good-produced-key (+ (good-produced-key home) produced)]
                      [:db/add farmer-eid good-produced-key (+ (good-produced-key factory) produced)]]))))]
    {:when '[[?e :money ?money]
             [?e :hometown ?home]
             [?e :farm-size ?farm-size]
             [?d :day ?day]]
     :then '[[:db.fn/call farm ?e ?day]]
     :call {'farm farm}}))

(def manage-stockpile-rule
  (let [process-stockpile (fn [db gov-eid]
                            (let [{:keys [governs money
                                          food-stockpile-buy? food-stockpile-sell? food-stockpile food-stockpile-buy-price food-stockpile-sell-price
                                          clothes-stockpile-buy? clothes-stockpile-sell? clothes-stockpile clothes-stockpile-buy-price clothes-stockpile-sell-price]
                                   :or {food-stockpile 0 food-stockpile-buy-price 1 food-stockpile-sell-price 1
                                        clothes-stockpile 0 clothes-stockpile-buy-price 1 clothes-stockpile-sell-price 1} :as gov} (d/entity db gov-eid)
                                  {food-market :food/market clothes-market :clothes/market governs-eid :db/id} governs
                                  enough-money? (>= money (* food-stockpile-buy-price 100))
                                  enough-food? (> food-stockpile 0)
                                  enough-clothes? (> clothes-stockpile 0)]
                              (log :info :process-stockpile :gov gov :governs governs)
                              (let [food-market' (cond-> food-market
                                                   (and food-stockpile-buy? enough-money?)
                                                   (load-order {:price food-stockpile-buy-price :size 100 :side :buys :id gov-eid :good-kw :food-stockpile})

                                                   (not food-stockpile-buy?)
                                                   (remove-order :buys gov-eid)

                                                   (and food-stockpile-sell? enough-food?)
                                                   (load-order {:price food-stockpile-sell-price :size (min 100 food-stockpile) :side :sell :id gov-eid :good-kw :food-stockpile})

                                                   (not (and food-stockpile-sell? enough-food?))
                                                   (remove-order :sell gov-eid))
                                    clothes-market' (cond-> clothes-market
                                                      (and clothes-stockpile-buy? enough-money?)
                                                      (load-order {:price clothes-stockpile-buy-price :size 100 :side :buys :id gov-eid :good-kw :clothes-stockpile})

                                                      (not clothes-stockpile-buy?)
                                                      (remove-order :buys gov-eid)

                                                      (and clothes-stockpile-sell? enough-clothes?)
                                                      (load-order {:price clothes-stockpile-sell-price :size (min 100 clothes-stockpile) :side :sell :id gov-eid :good-kw :clothes-stockpile})

                                                      (not (and clothes-stockpile-sell? enough-clothes?))
                                                      (remove-order :sell gov-eid))]
                                (log :debug :governs-eid governs-eid :update-food-market (not= food-market food-market')  :update-clothes-market (not= clothes-market clothes-market'))
                                (log :trace :food-market food-market' :clothes-market' clothes-market')
                                (cond-> []
                                  (not= food-market food-market')
                                  (conj [:db/add governs-eid :food/market food-market'])

                                  (not= clothes-market clothes-market')
                                  (conj [:db/add governs-eid :clothes/market clothes-market'])))))]
    {:when '[[?e :money]
             [(get-else $ ?e :food-stockpile-buy? false) ?food-buy?]
             [(get-else $ ?e :food-stockpile-sell? false) ?food-sell?]
             [(get-else $ ?e :clothes-stockpile-buy? false) ?clothes-buy?]
             [(get-else $ ?e :clothes-stockpile-sell? false) ?clothes-sell?]
             [(or ?food-buy? ?food-sell? ?clothes-buy? ?clothes-sell?)]]
     :then '[[:db.fn/call process-stockpile ?e]]
     :call {'process-stockpile process-stockpile}}))

(def infrastructure-hire-rule
  (let [hire-maintainers (fn [db gov-eid]
                           (let [{:keys [governs money
                                         infrastructure/maintenance] :as gov} (d/entity db gov-eid)
                                 {home-eid :db/id
                                  labour-price :labour/price
                                  labour-market :labour/market
                                  :as home} governs
                                 labour-price' (max labour-price 40)
                                 labour-want (min maintenance (quot (quot money 4) labour-price'))]
                             (log :debug :hire-maintainers gov-eid)
                             (log :trace :gov gov)
                             (when (and (> maintenance 0) (> labour-want 0))
                               [[:db/add home-eid :labour/market (load-order labour-market {:price labour-price' :size labour-want :side :buys :id gov-eid :good-kw :infrastructure/labour-bought})]])))]
    {:when '[[?e :infrastructure/maintenance]]
     :then '[[:db.fn/call hire-maintainers ?e]]
     :call {'hire-maintainers hire-maintainers}}))

(def infrastructure-fix-rule
  (let [infrastructure-fix (fn [db gov-eid]
                             (let [{:keys [infrastructure/maintenance
                                           infrastructure/labour-bought] :as gov} (d/entity db gov-eid)]
                               (log :debug :infrastructure-fix gov-eid)
                               (log :trace :gov gov)
                               (when (and (> maintenance 0) (> labour-bought 0))
                                 [[:db/add gov-eid :infrastructure/maintenance (max (- maintenance labour-bought) 0)]
                                  [:db/add gov-eid :infrastructure/labour-bought 0]])))]
    {:when '[[?e :infrastructure/maintenance]]
     :then '[[:db.fn/call infrastructure-fix ?e]]
     :call {'infrastructure-fix infrastructure-fix}}))

(def infrastructure-decay-rule
  (let [decay (fn [db gov-eid]
                (let [{:keys [infrastructure/size infrastructure/maintenance infrastructure/hp] :as gov} (d/entity db gov-eid)]
                  (log :debug :infrastructure-decay gov-eid)
                  (log :trace :gov gov)
                  (cond-> [[:db/add gov-eid :infrastructure/maintenance (* size 100)]]

                    (not (zero? maintenance))
                    (conj [:db/add gov-eid :infrastructure/hp (dec hp)]))))]
    {:when '[[?de :day ?day]
             [(mod ?day 30) ?day-mod]
             [(zero? ?day-mod)]
             [?e :infrastructure/maintenance]]
     :then '[[:db.fn/call decay ?e]]
     :call {'decay decay}}))


(defn update-attrs [db eid attr-fn-vals]
  (let [ent (d/entity db eid)]
    (log :trace :update-attrs (d/touch ent) attr-fn-vals)
    (into []
      (map (fn [[attr f val]]
             (log :trace eid attr (get ent attr))
             (log :trace (f (get ent attr) val))
             [:db/add eid attr (f (get ent attr) val)]))
      attr-fn-vals)))

(defn process-matched [db matches]
  (reduce
    (fn [tx {:keys [buyer seller size price] :as order}]
      (let [cost (* size price)
            bought-kw (get-in order [:buy-order :good-kw])
            sold-kw (get-in order [:sell-order :good-kw])]
        (log :debug :seller (d/touch (d/entity db seller)))
        (into tx
          [[:db.fn/call update-attrs seller [[:money + cost]
                                             [:sold + size]
                                             [:earned + cost]
                                             [sold-kw - size]]]
           [:db.fn/call update-attrs buyer [[:money - cost]
                                            [bought-kw + size]]]])))
    []
    matches))

;;(reset! *sim-broken nil)

;;(identity @*sim-broken)

(def match-markets-rule
  (let [match-market
        (fn [db town-eid]
          (log :info :match-market town-eid)
          (let [{food-market :food/market
                 clothes-market :clothes/market
                 labour-market :labour/market
                 :as town} (d/entity db town-eid)
                market->tx (fn [town-eid market {:keys [market-key demand-key supply-key market-price-key]}]
                             (let [{matched :matched :keys [sold current-price] :as market'} (-> market match-orders market-summary)]
                               (if (and (= market market') (empty? matched))
                                 ;; There's no change
                                 [(assoc market' :kind :trade :market market-key)]
                                 (let [process-matched-tx (process-matched db matched)
                                       market'' (assoc market' :matched [] :sold 0)
                                       [demand supply] ((juxt (comp count :buys) (comp count :sell)) market'')]
                                   (into process-matched-tx
                                     [[:db/add town-eid market-key market'']
                                      [:db/add town-eid demand-key demand]
                                      [:db/add town-eid supply-key supply]
                                      [:db/add town-eid market-price-key current-price]
                                      (assoc market' :kind :trade :market market-key)])))))]
            ;; TODO: Just have this running once? So we only have one market match, but for now maybe run it twice
            (log :debug :town (d/touch town))
            (into []
              cat
              [(when (seq food-market)
                 (market->tx town-eid food-market {:market-key :food/market :demand-key :food/demand :supply-key :food/supply :market-price-key :food/market-price}))
               (when (seq clothes-market)
                 (market->tx town-eid clothes-market {:market-key :clothes/market :demand-key :clothes/demand :supply-key :clothes/supply :market-price-key :clothes/market-price}))
               (when (seq labour-market)
                 (market->tx town-eid labour-market {:market-key :labour/market :demand-key :labour/demand :supply-key :labour/supply :market-price-key :labour/market-price}))])))]
    {:when '[(or
               [?e :food/market _]
               [?e :clothes/market _]
               [?e :labour/market _])]
     :then '[[:db.fn/call match-market ?e]]
     :call {'match-market match-market}}))

(defn conj-to-limit
  "conj's a value to a vector to the specified limit"
  [v limit x]
  (let [v' (conj v x)
        size (count v')]
    (if (> size limit)
      (subvec v' (- size limit) size)
      v')))

(def price-history-limit (* 30 36))
(def max-price (long 1e12))

(def update-prices-rule
  (let [;; Post some experimentation growth factor should probably sit between 1.0001 and 1.001,
        ;;   which gives us between 5.4% and 37.78% price increase in a year in conditions of constant demand.
        ;;   We'll set it at 1.0005 for now, which is 11.35%.
        ;; I'll defer doing the same for the deflation until I have a better situation to build a model for it.
        inflate-by 1.2 #_1.0005
        deflate-by -0.5
        update-price-velocity (fn [supply demand price-velocity]
                                (let [more-demand? (> demand supply)
                                      more-supply? (< demand supply)]
                                  (cond
                                    (and more-demand? (> price-velocity 0))
                                    ;; TODO: use whole numbers instead of
                                    (max (* price-velocity inflate-by) 0.01)

                                    more-demand?
                                    (max (* price-velocity deflate-by) 0.01)

                                    (and more-supply? (< price-velocity 0))
                                    (min (* price-velocity inflate-by) -0.01)

                                    more-supply?
                                    (min (* price-velocity deflate-by) -0.01)

                                    :else 0)))
        #_#_
        update-price-velocity (fn [supply demand price-velocity]
                                (let [more-demand? (> demand supply)
                                      more-supply? (< demand supply)]
                                  (cond
                                    (and more-demand? (> price-velocity 0))
                                    (max (+ (quot price-velocity 2) price-velocity) 1)

                                    more-demand?
                                    (max (- (quot price-velocity 2)) 1)

                                    (and more-supply? (< price-velocity 0))
                                    (min (+ (quot price-velocity 2) price-velocity) -1)

                                    more-supply?
                                    (min (- (quot price-velocity 2)) -1)

                                    :else 0)))
        update-prices (fn [db town-eid]
                        (let [{food-price-float :food/price-float food-price-velocity :food/price-velocity food-price :food/price food-market-price :food/market-price food-price-history :food/price-history food-supply :food/supply food-demand :food/demand food-produced :food/produced food-consumed :food/consumed
                               clothes-price-float :clothes/price-float clothes-price-velocity :clothes/price-velocity clothes-price :clothes/price clothes-market-price :clothes/market-price clothes-price-history :clothes/price-history clothes-supply :clothes/supply clothes-demand :clothes/demand clothes-produced :clothes/produced clothes-consumed :clothes/consumed
                               labour-price-float :labour/price-float labour-price-velocity :labour/price-velocity labour-price :labour/price labour-market-price :labour/market-price labour-price-history :labour/price-history labour-supply :labour/supply labour-demand :labour/demand labour-produced :labour/produced labour-consumed :labour/consumed
                               :as town} (d/entity db town-eid)
                              food-price-velocity' (update-price-velocity food-supply food-demand food-price-velocity)
                              food-price-float' (+ food-price-float food-price-velocity')
                              clothes-price-velocity' (update-price-velocity clothes-supply clothes-demand clothes-price-velocity)
                              clothes-price-float' (+ clothes-price-float clothes-price-velocity')
                              labour-price-velocity' (update-price-velocity labour-supply labour-demand labour-price-velocity)
                              labour-price-float' (+ labour-price-float labour-price-velocity')

                              #_#_#_#_#_#_#_#_#_#_#_#_
                              food-price-float'' (min (max food-price-float' 1) max-price)
                              food-price' (min (max (long food-price-float') 1) max-price)
                              clothes-price-float'' (min (max clothes-price-float' 1) max-price)
                              clothes-price' (min (max (long clothes-price-float') 1) max-price)
                              labour-price-float'' (min (max labour-price-float' 1) max-price)
                              labour-price' (min (max (long labour-price-float') 1) max-price)]
                          (log :debug :update-prices :town-eid town-eid :labour-supply labour-supply :labour-demand labour-demand :labour-price-velocity labour-price-velocity :labour-price-velocity' labour-price-velocity' :labour-price-float labour-price-float :labour-price-float' labour-price-float' :labour-market-price labour-market-price)
                          (log :debug :update-prices :food/produced food-produced :food/consumed food-consumed :clothes/produced clothes-produced :clothes/consumed clothes-consumed :labour/produced labour-produced :labour/consumed labour-consumed)
                          [[:db/add town-eid :food/price-velocity food-price-velocity']
                           #_[:db/add town-eid :food/price-float (max food-price-float' 1)]
                           #_[:db/add town-eid :food/price (max (quot food-price-float' 100) 1)]
                           ;;[:db/add town-eid :food/supply 0]
                           ;;[:db/add town-eid :food/demand 0]
                           ;;[:db/add town-eid :food/last-supply food-supply]
                           ;;[:db/add town-eid :food/last-demand food-demand]
                           ;;[:db/add town-eid :food/produced 0]
                           ;;[:db/add town-eid :food/consumed 0]
                           ;;[:db/add town-eid :food/last-produced food-produced]
                           ;;[:db/add town-eid :food/last-consumed food-consumed]
                           [:db/add town-eid :clothes/price-velocity clothes-price-velocity']
                           #_[:db/add town-eid :clothes/price-float (max clothes-price-float' 1)]
                           #_[:db/add town-eid :clothes/price (max (quot clothes-price-float' 100) 1)]
                           ;;[:db/add town-eid :clothes/supply 0]
                           ;;[:db/add town-eid :clothes/demand 0]
                           ;;[:db/add town-eid :clothes/last-supply clothes-supply]
                           ;;[:db/add town-eid :clothes/last-demand clothes-demand]
                           ;;[:db/add town-eid :clothes/produced 0]
                           ;;[:db/add town-eid :clothes/consumed 0]
                           ;;[:db/add town-eid :clothes/last-produced clothes-produced]
                           ;;[:db/add town-eid :clothes/last-consumed clothes-consumed]
                           [:db/add town-eid :labour/price-velocity labour-price-velocity']
                           #_[:db/add town-eid :labour/price-float (max labour-price-float' 1)]
                           #_[:db/add town-eid :labour/price (max (quot labour-price-float' 100) 1)]
                           ;;[:db/add town-eid :labour/supply 0]
                           ;;[:db/add town-eid :labour/demand 0]
                           ;;[:db/add town-eid :labour/last-supply labour-supply]
                           ;;[:db/add town-eid :labour/last-demand labour-demand]
                           ;;[:db/add town-eid :labour/produced 0]
                           ;;[:db/add town-eid :labour/consumed 0]
                           ;;[:db/add town-eid :labour/last-produced labour-produced]
                           ;;[:db/add town-eid :labour/last-consumed labour-consumed]

                           ;#_#_#_#_#_#_#_#_#_
                           #_#_
                           [:db/add town-eid :food/price-float food-price-float'']
                           [:db/add town-eid :food/price food-price']
                           [:db/add town-eid :food/price-history (conj-to-limit food-price-history price-history-limit food-price)]
                           #_#_
                           [:db/add town-eid :clothes/price-float clothes-price-float'']
                           [:db/add town-eid :clothes/price clothes-price']
                           [:db/add town-eid :clothes/price-history (conj-to-limit clothes-price-history price-history-limit clothes-price)]
                           #_#_
                           [:db/add town-eid :labour/price-float labour-price-float'']
                           [:db/add town-eid :labour/price labour-price']
                           [:db/add town-eid :labour/price-history (conj-to-limit labour-price-history price-history-limit labour-price)]

                           [:db/add town-eid :food/price food-market-price]
                           [:db/add town-eid :clothes/price clothes-market-price]
                           [:db/add town-eid :labour/price labour-market-price]]))]

    {:when '[[?e :food/price _]]
     :then '[[:db.fn/call update-prices ?e]]
     :call {'update-prices update-prices}}))

(defn gen-tax-tx [gov-ent gov-money tax-rate tax-day? citizens]
  (let [{:keys [pay-tx tax-earnings]} (reduce
                                        (fn [state ent]
                                          (let [ent-id (:db/id ent)
                                                money (:money ent)
                                                earned (:earned ent)
                                                owed-tax (:earned ent)
                                                tax (long (* tax-rate earned))
                                                owed-tax' (+ owed-tax tax)
                                                peep-tax-tx (cond-> [[:db/add ent-id :money (- money tax)]
                                                                     [:db/add ent-id :earned 0]
                                                                     [:db/add ent-id :last-earned earned]]
                                                              (not tax-day?)
                                                              (conj [:db/add ent-id :owed-tax owed-tax'])
                                                              tax-day?
                                                              (conj [:db/add ent-id :owed-tax 0]))]
                                            (log :debug :money-tax! :eid ent-id :money money :tax tax :ent (touch ent))
                                            (-> state
                                              (update :pay-tx into peep-tax-tx)
                                              (cond->
                                                tax-day?
                                                (update :tax-earnings + owed-tax')))))
                                        {:pay-tx [] :tax-earnings 0}
                                        citizens)]
    (conj pay-tx [:db/add gov-ent :money (+ gov-money tax-earnings)])))

(def state-tax-rule
  (let [tax (fn [db day]
              (let [governments (lookup-avet db :governs nil)
                    tax-day? (zero? (mod day 30))]
                (log :debug :state-tax)
                (reduce
                  (fn [v {gov-ent :db/id
                          gov-money :money
                          :keys [governs tax-rate] :as gov}]
                    (let [tax-rate (/ tax-rate 100)
                          tax-tx (gen-tax-tx gov-ent gov-money tax-rate tax-day? (:_hometown governs))]
                      (log :trace (mapv (juxt :db/id :money) (:_hometown governs)) (:governs gov) tax-tx)
                      (into v tax-tx)))
                  []
                  governments)))]
    {:when '[[?e :day ?day]]
     :then '[[:db.fn/call tax ?day]]
     :call {'tax tax}}))

(comment
  (let [broken @*sim-broken
        world @state/*world
        world-db (:world-db world)
        governments (lookup-avet world-db :governs nil)]
    (reduce
      (fn [v {gov-ent :db/id
              gov-money :money
              :keys [governs tax-rate] :as gov}]
        (let [tax-rate (/ tax-rate 100)
              {:keys [tax-tx gov-money]} (reduce
                                           (fn [state ent]
                                             (let [money (:money ent)
                                                   tax (long (* tax-rate money))]
                                               (-> state
                                                 (update :tax-tx conj [:db/add (:db/id ent) :money (- money tax)])
                                                 (update :gov-money + tax))))
                                           {:tax-tx [] :gov-money gov-money}
                                           (:_hometown governs))
              tx (conj tax-tx [:db/add gov-ent :money gov-money])]
          (log :trace (mapv (juxt :db/id :money) (:_hometown governs)) (:governs gov) tax-tx)
          (into v tx)))
      []
      governments)))

(def relocate-rule)


(def decision-rules
  [start-rule
   days-pass-rule
   hunt-rule
   try-eat-rule
   grow-food-rule

   create-settlement-rule
   peep-shop-rule
   peep-consume-rule
   adjust-factories-planning-rule
   hire-rule
   craft-rule
   ;;farmers-farm-rule
   manage-stockpile-rule
   infrastructure-hire-rule
   match-markets-rule
   infrastructure-fix-rule
   infrastructure-decay-rule
   update-prices-rule
   reset-entity-rule
   state-tax-rule])

(def reaction-rules
  [remove-starving-rule])

(def create-settlement-rule-2
  {:when '[[?e :wealth ?wealth]
           [(get-else $ ?e :settlement :db/missing) ?s]
           [(= :db/missing ?s)]
           [(>= ?wealth 10)]
           [(- ?wealth 10) ?rem-wealth]
           [?e :place ?place]
           [(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ?letter]
           [(str ?letter) ?name]]
   :then (into
           '[[:db/add ?e :wealth ?rem-wealth]
             [:db/add ?e :settlement -1]
             [:db/add -1 :kind :city]
             [:db/add -1 :settlement/name ?name]
             [:db/add -1 :settlement/place ?place]]
           (comp cat cat)
           [;; peeps
            (for [idx (range 10)]
              [[:db/add (- idx 100) :kind :peep]
               [:db/add (- idx 100) :hometown -1]
               [:db/add (- idx 100) :money 10000]
               [:db/add (- idx 100) :planning (rand-nth [1 3 5 8 15])]
               [:db/add (- idx 100) :inventory {}]
               [:db/add (- idx 100) :needs {:food-base 9 #_#_#_#_:food-extra 9 :drink-base 9}]])])
   :args {'rand-nth rand-nth}})

(def forage-rule-2
  (let [forage (fn [db eid]
                 (let [{:keys [hometown inventory] :as entity} (d/entity db eid)
                       coord (get-in hometown [:settlement/place :coord])
                       findable (sees coord 5)
                       nearby-biomes (frequencies
                                       (into [] (comp
                                                  (map (fn [coord] (lookup-avet db :coord coord)))
                                                  cat
                                                  (map :biome)) findable))
                       collected (weighted-rand nearby-biomes)
                       good (case collected
                              :beach :beach-fruit :snow-mountain :snow-mountain-fruit
                              :mountain :mountain-fruit :snow :snow-fruit
                              :tundra :tundra-fruit :grassland :grassland-fruit
                              :forest :forest-fruit :jungle :jungle-fruit
                              :desert :desert-fruit :ocean :ocean-fruit)]
                   [[:db/add eid :inventory (update inventory good (fnil + 0) 10)]]))]
    {:when '[[?e :kind :peep]
             [?e :inventory]]
     :then '[[:db.fn/call forage ?e]]
     :call {'forage forage}}))

;; We have some number of peeps
;; - they just have needs, which we can decide how to support.
;; - We can incentivise peeps

(comment
  (let [day-of-food (apply merge-with + (repeat 2 {:food-base 9 :food-extra 9 :drink-base 9}))
        pantry {:food-extra {"Eggs" {:name     "Eggs",
                                     :category :food-extra,
                                     :portions 168}},
                :food-base  {"Hard Bread" {:name     "Hard Bread",
                                           :category :food-base,
                                           :portions 126}},
                :drink-base {"Cheap Beer" {:name     "Cheap Beer",
                                           :category :drink-base,
                                           :portions 156}}}]
    (let [inventory {:desert-fruit 12 :mountain-fruit 1}
          items (set (keys inventory))
          all-fruits #{:beach-fruit :snow-mountain-fruit
                       :mountain-fruit :snow-fruit
                       :tundra-fruit :grassland-fruit
                       :forest-fruit :jungle-fruit
                       :desert-fruit :ocean-fruit}
          category->items {:food-base #{:beach-fruit :desert-fruit}
                           :drink-base all-fruits
                           :food-extra all-fruits}]
      (reduce-kv
        (fn [m k v]
          (println m k v (set/intersection (category->items k) items))
          (println (first (set/intersection (category->items k) items)))
          (if-let [k' (first (set/intersection (category->items k) items))]
            (update m k' - v)
            m))
        inventory
        {:food-base 9 :food-extra 9 :drink-base 9}))

    #_(reduce-kv (fn [pantry food-category portions-required]
                   (let [update-category (fn [pantry-category]
                                           (-> (reduce-kv
                                                 (fn [{:keys [pantry-cat portions-to-eat] :as state} food-name {:keys [portions] :as food-detail}]
                                                   (println (<= portions-to-eat portions) :pantry-cat pantry-cat :portions-to-eat portions-to-eat :food-name food-name :food-detail food-detail)
                                                   (if (<= portions-to-eat portions)
                                                     (reduced (-> state
                                                                  (update-in [:pantry-cat food-name :portions] - portions-to-eat)
                                                                  (assoc :portions-to-eat 0)))
                                                     (-> state
                                                         (update :pantry-cat dissoc food-name)
                                                         (update state :portions-to-eat - portions))))
                                                 {:pantry-cat pantry-category
                                                  :portions-to-eat portions-required}
                                                 pantry-category)
                                               :pantry-cat))]
                     (update pantry food-category update-category)))
        pantry day-of-food)))

(def peep-consume-rule-2
  (let [consume (fn [db peep-eid]
                  (let [{:keys [needs] :as peep} (d/entity db peep-eid)]
                    (println :needs needs)))]
    {:when '[[?e :needs]]
     :then '[[:db.fn/call consume ?e]]
     :call {'consume consume}}))


(comment
  (let [goods (map :inventory
                (lookup-avet (:world-db @state/*world) :kind :peep))
        inventory (apply merge-with + goods)]
    (update-vals inventory (fn [v] (/ 1.0 v)))
    #_
    (mapv
      (fn [good]
        (reduce-kv
          (fn [m k v]
            (assoc m k (float (/ v (get inventory k)))))
          {}
          good))
      goods)))
#_
(def decision-rules
  [days-pass-rule
   hunt-rule
   try-eat-rule
   grow-food-rule

   create-settlement-rule-2
   forage-rule-2
   peep-consume-rule-2])

(def *sim-broken (atom nil))

(defn tick-world [*world]
  (tufte/p :tick-world
    (if (nil? @*sim-broken)
      (try
        (as-> *world $
          (update $ :world-conn apply-rules decision-rules reaction-rules)
          (assoc $ :world-db (d/db (:world-conn $)))
          (assoc $ :day (lookup-day (:world-db $)))
          (update $ :dbs (fnil track-db []) (:world-db $))
          (update $ :stats (fnil add-stats []) (:world-db $)))
        (catch Exception e
          (println :BROKE!)
          (reset! *sim-broken e)
          *world))
      *world)))

(defn transact-with-db [*world tx-data]
  (try
    (as-> *world $
      (update $ :world-db d/db-with tx-data))
    (catch Exception e
      (println :BROKE!)
      (reset! *sim-broken e)
      *world)))

(defn transact-with-conn [*world tx-data]
  (try
    (let [conn (:world-conn *world)]
      (d/transact! conn tx-data)
      (assoc *world :world-db (d/db conn)))
    (catch Exception e
      (println :BROKE!)
      (reset! *sim-broken e)
      *world)
    (catch AssertionError e
      (println :BROKE!)
      (reset! *sim-broken e)
      *world)))

(comment
  (identity @*sim-broken)

  (require '[flow-storm.api :as fs-api])

  (fs-api/local-connect)
  (fs-api/instrument-forms-for-namespaces #{"fruit-economy.sim.basic" "fruit-economy.sim.market"} {})
  (fs-api/instrument-forms-for-namespaces #{"posh.core" "posh.clj.datascript" "posh.lib.q-analyse"} {})
  (fs-api/instrument-var 'fruit-economy.sim.basic/rewrite)
  (fs-api/instrument-var 'fruit-economy.sim.basic/infer)

  ;;(do #rtrace
  ;;    (tick-world @state/*world)
  ;;  nil)

  ,)

(defn do-tick-world []
  (swap! state/*world tick-world))

(defn tick-world-10x []
  (dotimes [_ 10]
    (do-tick-world)))

(defn tick-world-100x []
  (dotimes [_ 100]
    (do-tick-world)))

(defn tick-world-1000x []
  (dotimes [_ 1000]
    (do-tick-world)))

(defn history-started? [world-db]
  (< 30 (lookup-day world-db)))

(defn viable-world? [world-db]
  (seq (lookup-avet world-db :kind :city)))


;;#rtrace
(do
  (taoensso.timbre/with-level :trace
    (infer (:world-db @state/*world)
      decision-rules
      #_[peep-consume-rule
         hire-rule
         match-markets-rule]
      ;;[farmers-farm-rule]
      1))
  nil)

(comment
  (let [world-db (:world-db @state/*world)]
    (->> (d/q
           '[:find [?e ...]
             :where [?e :food-stockpile ?s]]
           world-db)
      (mapv (comp d/touch #(d/entity world-db %))))))

(comment
  #_[:dali/page
     [:circle
      {:stroke :indigo :stroke-width 4 :fill :darkorange}
      [30 30] 20]]
  #_"<svg>
    <polyline
       fill=\"none\"
       stroke=\"#0074d9\"
       stroke-width=\"3\"
       points=\"
       0,120
       20,60
       40,80
       60,20\"/>
  </svg>"
  #_"<svg version=\"1.2\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
      <title id=\"title\">A line chart showing some information</title>
    <g class=\"grid x-grid\" id=\"xGrid\">
      <line x1=\"90\" x2=\"90\" y1=\"5\" y2=\"371\"></line>
    </g>
    <g class=\"grid y-grid\" id=\"yGrid\">
      <line x1=\"90\" x2=\"705\" y1=\"370\" y2=\"370\"></line>
    </g>
      <g class=\"labels x-labels\">
      <text x=\"100\" y=\"400\">2008</text>
      <text x=\"246\" y=\"400\">2009</text>
      <text x=\"392\" y=\"400\">2010</text>
      <text x=\"538\" y=\"400\">2011</text>
      <text x=\"684\" y=\"400\">2012</text>
      <text x=\"400\" y=\"440\" class=\"label-title\">Year</text>
    </g>
    <g class=\"labels y-labels\">
      <text x=\"80\" y=\"15\">15</text>
      <text x=\"80\" y=\"131\">10</text>
      <text x=\"80\" y=\"248\">5</text>
      <text x=\"80\" y=\"373\">0</text>
      <text x=\"50\" y=\"200\" class=\"label-title\">Price</text>
    </g>
    </svg>")

(comment
  (let [db (d/db-with (d/empty-db)
             [{:food 10 :clothes 10 :min-food 2 :min-clothes 1}])]
    (rewrite
      peep-consume-rule
      (infer db [peep-consume-rule] 1))
    #_(d/db-with db
        [[:db.fn/call transfer 1 3 100]]))

  (let [db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
             [{:food/price 1 :food/price-float 1.0 :food/demand 0 :food/supply 0 :food/price-velocity 0
               :clothes/price 2 :clothes/price-float 2.0 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0
               :labour/price 3 :labour/price-float 3.0 :labour/demand 0 :labour/supply 0 :labour/price-velocity 0}
              {:food/price 4 :food/price-float 4.0 :food/demand 1 :food/supply 0 :food/price-velocity 0
               :clothes/price 5 :clothes/price-float 5.0 :clothes/demand 1 :clothes/supply 0 :clothes/price-velocity 0
               :labour/price 6 :labour/price-float 6.0 :labour/demand 1 :labour/supply 0 :labour/price-velocity 0}
              {:food/price 7 :food/price-float 7.0 :food/demand 0 :food/supply 1 :food/price-velocity 0
               :clothes/price 8 :clothes/price-float 8.0 :clothes/demand 0 :clothes/supply 1 :clothes/price-velocity 0
               :labour/price 9 :labour/price-float 9.0 :labour/demand 0 :labour/supply 1 :labour/price-velocity 0}])]
    (infer db
      [update-prices-rule]
      1))

  (let [db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
             [{:food/price 1 :food/demand 1 :food/supply 1
               :clothes/price 1 :clothes/demand 1 :clothes/supply 1
               :labour/supply 1}
              {:food 10 :clothes 10 :min-food 2 :min-clothes 1 :hometown 1}])]
    (infer db [peep-shop-rule
               peep-consume-rule] 1))

  (let [db (:world-db @state/*world)]
    (rewrite
      peep-consume-rule
      (infer db [peep-consume-rule] 1)))

  (let [db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}
                                   :good {:db/index true}
                                   :kind {:db/index true}})
             [{:labour/price 1 :labour/demand 1 :labour/supply 1 :db/id 1}
              {:good :clothes :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 15 :hometown 1}
              {:kind :peep :money 10000 :labour 10 :health 10 :food 0 :clothes 0 :min-food 2 :min-clothes 1 :planning 1 :hometown 1}
              {:kind :peep :money 10000 :labour 0 :health 10 :food 0 :clothes 0 :min-food 2 :min-clothes 1 :planning 1 :hometown 1}
              {:kind :peep :money 10000 :labour 10 :health 10 :food 0 :clothes 0 :min-food 2 :min-clothes 1 :planning 1 :hometown 1}])
        craft (fn [db factory-eid]
                (let [{:keys [money planning min-labour inventory decay production] :as factory} (d/entity db factory-eid)
                      labour-plan (* min-labour planning)
                      {labour-price :labour/price
                       labour-supply :labour/supply
                       :as home} (get factory :hometown)
                      home-eid (:db/id home)
                      labour-like (like-to-buy money labour-price 0.8)
                      labour-want (min labour-like labour-plan)
                      labour-bought (min labour-want labour-supply)
                      labour-cost (* labour-bought labour-price)
                      produced (long (* production (Math/log (+ labour-bought 1))))
                      ;; TODO: Decay should probably be it's own rule
                      decayed-inventory (long (* inventory decay))
                      mixed-peeps (->> #_(lookup-avet db :kind :peep)
                                    (d/q '[:find ?e ?labour
                                           :in $ ?home-eid
                                           :where
                                           [?e :kind :peep]
                                           [?e :hometown ?home-eid]
                                           [?e :labour ?labour]
                                           [(> ?labour 0)]]
                                      db home-eid)
                                    (shuffle))]
                  (println :mixed-peeps mixed-peeps)
                  (println factory-eid :craft labour-want :labour-bought labour-bought (d/touch factory))
                  (cond-> [[:db/add home-eid :labour/demand (+ (:labour/demand home) labour-want)]
                           [:db/add factory-eid :inventory (+ decayed-inventory produced)]]

                    (>= labour-bought min-labour)
                    (into [[:db/add home-eid :labour/supply (- (:labour/supply home) labour-supply)]
                           [:db/add factory-eid :money (- money labour-cost)]]))))]
    (infer db
      [{:when '[[?e :money ?money]
                [?e :hometown ?home]
                [?e :inventory ?inventory]
                [(< ?inventory 100)]]
        :then '[[:db.fn/call craft ?e]]
        :call {'craft craft}}]
      1))


  (let [db (:world-db @state/*world)
        budget (fn [money food-price clothes-price]
                 (let [food-budget (int (* money 0.4))
                       clothes-budget (int (* money 0.2))
                       food-buy (quot food-budget food-price)
                       clothes-buy (quot clothes-budget clothes-price)]
                   [food-buy (* food-buy food-price) clothes-buy (* clothes-buy clothes-price)]))]
    #_(lookup-avet db :settlement/place nil)
    (d/q
      '[:find (pull ?e [* {:hometown [*]}]) ?food-cost ?clothes-cost ?rem-money
        :in $ budget
        :where
        [?e :money ?money]
        [?e :hometown ?home]
        [?home :food/price ?food-price]
        [?home :clothes/price ?clothes-price]
        [(budget ?money ?food-price ?clothes-price) [?food-buy ?food-cost ?clothes-buy ?clothes-cost]]
        [(- ?money ?food-cost ?clothes-cost) ?rem-money]]
      db
      budget))

  (let [db (:world-db @state/*world)
        budget (fn [money food-price clothes-price]
                 (let [food-budget (int (* money 0.4))
                       clothes-budget (int (* money 0.2))
                       food-buy (quot food-budget food-price)
                       clothes-buy (quot clothes-budget clothes-price)]
                   [food-buy (* food-buy food-price) clothes-buy (* clothes-buy clothes-price)]))
        rules
        [{:when '[[?e :money ?money]
                  [?e :hometown ?home]
                  [?home :food/price ?food-price]
                  [?home :clothes/price ?clothes-price]
                  [(budget ?money ?food-price ?clothes-price) [?food-buy ?food-cost ?clothes-buy ?clothes-cost]]
                  [(- ?money ?food-cost ?clothes-cost) ?rem-money]
                  [(>= ?rem-money 0)]
                  [?home :food/demand ?food-demand]
                  [?home :clothes/demand ?clothes-demand]
                  [(+ ?food-demand ?food-buy) ?food-demand']
                  [(+ ?clothes-demand ?clothes-buy) ?clothes-demand']]
          :then '[[:db/add ?home :food/demand ?food-demand']
                  [:db/add ?home :clothes/demand ?clothes-demand']
                  [?e :money ?rem-money]]
          :args {'budget budget}}]]
    (rewrite (first rules) db))

  (let [db (:world-db @state/*world)
        budget (fn [money food-price clothes-price]
                 (let [food-budget (int (* money 0.4))
                       clothes-budget (int (* money 0.2))
                       food-like (quot food-budget food-price)
                       clothes-like (quot clothes-budget clothes-price)]
                   [food-like clothes-like]))
        shop
        (fn [db peep-eid]
          (let [{:keys [money planning food clothes] :as peep} (d/entity db peep-eid)
                min-food 2 min-clothes 1
                food-plan (* min-food planning) clothes-plan (* min-clothes planning)
                {food-price :food/price
                 clothes-price :clothes/price
                 :as home} (get peep :hometown)
                [food-like clothes-like] (budget money food-price clothes-price)
                food-want (min food-like food-plan) clothes-want (min clothes-like clothes-plan)
                food-cost (* food-like food-price) clothes-cost (* clothes-like clothes-price)
                food-bought food-want clothes-bought clothes-want]
            (println :shop food-want clothes-want (d/touch peep))
            [[:db/add (:db/id home) :food/demand (+ (:food/demand home) food-want)]
             [:db/add (:db/id home) :clothes/demand (+ (:clothes/demand home) clothes-want)]
             [:db/add peep-eid :money (- money food-cost clothes-cost)]
             [:db/add peep-eid :food (+ food food-bought)]
             [:db/add peep-eid :clothes (+ clothes clothes-bought)]]))
        rules
        [{:when '[[?e :money ?money]
                  [?e :hometown ?home]
                  [?e :food ?food]
                  [?e :clothes ?clothes]
                  ;; remove so we can reduce shopping
                  ;[(<= ?food 2) ?enough-food]
                  ;[(<= ?clothes 1) ?enough-clothes]
                  ;[(or ?enough-food ?enough-clothes)]
                  [?home :food/price ?food-price]
                  [?home :clothes/price ?clothes-price]]
          :then '[[:db.fn/call shop ?e]]
          :args {'budget budget}
          :call {'shop shop}}]]
    (do
      (infer db rules 1)
      nil))
  (let [db (d/db-with (d/empty-db)
             [{:buyer 1 :money 100}
              {:buyer 2 :money 100}
              {:seller 1 :money 0}])
        transfer
        (fn [db be se amount]
          (println :transfer be se amount db)
          (let [buyer (d/entity db be)
                seller (d/entity db se)]
            [[:db/add be :money (- (:money buyer) amount)]
             [:db/add se :money (+ (:money seller) amount)]]))]
    (infer
      db
      [{:when '[[?be :buyer ?buyer]
                [?be :money ?bmoney]
                [(>= 100 ?bmoney)]
                [?se :seller ?smoney]]
        :then '[[:db.fn/call transfer ?be ?se 100]]
        :call {'transfer transfer}}]
      1))

  (let [db (d/db-with (d/empty-db)
             [{:buyer 1 :money 100}
              {:buyer 2 :money 100}
              {:seller 1 :money 0}])
        transfer (fn [db be se amount]
                   (println be se amount db))]
    (d/db-with db
      [[:db.fn/call transfer 1 3 100]])))

(comment
  (let [world-data (into
                     [{:day 0}
                      {:db/id -1 :kind :city
                       :food/price 1 :food/price-float 1.0 :food/demand 40000 :food/supply 70 :food/last-demand 40000 :food/last-supply 70 :food/price-velocity 0 :food/produced 0 :food/consumed 0
                       :clothes/price 1 :clothes/price-float 1.0 :clothes/demand 20000 :clothes/supply 10 :clothes/last-demand 20000 :clothes/last-supply 10 :clothes/price-velocity 0 :clothes/produced 0 :clothes/consumed 0
                       :labour/price 1 :labour/price-float 1.0 :labour/demand 160000 :labour/supply 50 :labour/last-demand 160000 :labour/last-supply 50 :labour/price-velocity 0 :labour/produced 0 :labour/consumed 0}]
                     cat
                     [(for [_ (range 5)]
                        {:kind :peep :money 10000 :labour 10 :health 10 :food 0 :clothes 0 :min-food 2 :min-clothes 1 :planning 1 :hometown -1})
                      (for [_ (range 5)]
                        {:kind :food-factory :good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 1 :hometown -1})
                      (for [_ (range 5)]
                        {:kind :clothes-factory :good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 4 :planning 1 :hometown -1})])
        db (d/db-with (d/empty-db {:day {:db/index true}
                                   :money {:db/index true}
                                   :kind {:db/index true}
                                   :good {:db/index true}
                                   :place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}
                                   :settlement/place {:db/valueType :db.type/ref}
                                   :hometown {:db/valueType :db.type/ref}})
             world-data)
        print-summary (fn [db]
                        (let [peeps (lookup-avet db :kind :peep)
                              food-factories (lookup-avet db :kind :food-factory)
                              clothes-factories (lookup-avet db :kind :clothes-factory)
                              cities (lookup-avet db :kind :city)
                              peep-keys (into [] (comp (map keys) cat (distinct)) peeps)
                              food-factory-keys (into [] (comp (map keys) cat (distinct)) food-factories)
                              clothes-factory-keys (into [] (comp (map keys) cat (distinct)) clothes-factories)
                              city-keys (into [] (comp (map keys) cat (distinct)) cities)
                              market-keys [[:market :price :price-float :price-velocity :supply :demand]
                                           [:food :food/price :food/price-float :food/price-velocity :food/last-supply :food/last-demand]
                                           [:cloth :clothes/price :clothes/price-float :clothes/price-velocity :clothes/last-supply :clothes/last-demand]
                                           [:labour :labour/price :labour/price-float :labour/price-velocity :labour/last-supply :labour/last-demand]]
                              money (lookup-avet db :money nil)
                              tick (lookup-day db)
                              tableise (fn [m k]
                                         (let [[k v] (if (vector? k) [(first k) (get m (second k) (second k))] [k (get m k k)])]
                                           (format (str "%" (count (str k)) "s") v)))
                              show-peep-summary false
                              show-food-factory-summary false
                              show-clothes-factory-summary false
                              show-city-summary true
                              show-stats-summary true]
                          (println (str "--- BEGIN TURN " tick " ---"))
                          (when show-peep-summary
                            (println "  |--- BEGIN PEEPS ---")
                            (println peep-keys)
                            (doseq [peep peeps]
                              (println (mapv (partial tableise peep) peep-keys)))
                            (println "  |--- END PEEPS ---"))
                          (when show-food-factory-summary
                            (println "  |--- BEGIN FOOD FACTORIES ---")
                            (println food-factory-keys)
                            (doseq [food-factory food-factories]
                              (println (mapv (partial tableise food-factory) food-factory-keys)))
                            (println "  |--- END FOOD FACTORIES ---"))
                          (when show-clothes-factory-summary
                            (println "  |--- BEGIN CLOTHES FACTORIES ---")
                            (println clothes-factory-keys)
                            (doseq [clothes-factory clothes-factories]
                              (println (mapv (partial tableise clothes-factory) clothes-factory-keys)))
                            (println "  |--- END CLOTHES FACTORIES ---"))
                          (when show-city-summary
                            (println "  |--- BEGIN CITIES ---")
                            #_#_
                            (println city-keys
                              (doseq [city cities]
                                (println (mapv (partial tableise city) city-keys))))
                            (println (select-keys (first cities) [:clothes/last-supply]))
                            (doseq [market market-keys
                                    :let [city (first cities)
                                          mks (first market-keys)]]
                              (println (mapv (partial tableise city) (map vector mks market))))
                            (println "  |--- END CITIES ---"))
                          (when show-stats-summary
                            (println "  |--- BEGIN STATS ---")
                            (println "Day:" tick)
                            (println "Peeps:" (count peeps))
                            (println "Food Factories:" (count food-factories))
                            (println "Clothes Factories:" (count clothes-factories))
                            (println "Money Supply:" (reduce + (mapv :money money)))
                            (println "  |--- END STATS ---"))
                          (println (str "--- END TURN " tick " ---"))))
        day-rule {:when '[[?e :day ?day]
                          [(inc ?day) ?next-day]]
                  :then '[[:db/add ?e :day ?next-day]]}
        _ (print-summary db)
        result (reduce
                 (fn [db' tick]
                   (let [;; suppress-println
                         db'' (binding [*out* (new java.io.StringWriter)]
                                (infer db'
                                  [peep-shop-rule
                                   peep-consume-rule
                                   craft-rule
                                   update-prices-rule
                                   day-rule]
                                  1))]
                     (print-summary db'')
                     db''))
                 db
                 (range 5 #_3))]))

(defn transpose [coll]
  (persistent!
    (reduce
      (fn [acc e]
        (reduce-kv
          (fn [acc2 i2 e2]
            (assoc! acc2 i2 ((fnil conj []) (get acc2 i2) e2)))
          acc
          e))
      (transient [])
      coll)))

(comment
  (do
    (def *test-world (atom {:world-db (reset-world-db) :map-view :default-view}))
    (reset! *sim-broken nil))
  (prof/profile {:return-file true})
  ;;"Elapsed time: 16005.221436 msecs"
  (with-redefs-fn {}
    #(time
       (let [limit #_30 (* 365 10) #_40 #_365
             init-limit 30
             commit? false]
         (loop [n 0
                world @*test-world]
           (let [cities (count (lookup-avet (:world-db world) :kind :city))]
             (println "DAY:" n cities)
             (cond
               (or (< n init-limit) (and (< n limit) (not (zero? cities))))
               (recur
                 (inc n)
                 (binding [*out* (new java.io.StringWriter)]
                   (tick-world world)))

               :else (when commit?
                       (reset! *test-world world))))))))

  #_
  (dotimes [_ 30 #_(* 365 10)]
    (binding [*out* (new java.io.StringWriter)]
      (swap! *test-world tick-world))
    (println (count (lookup-avet (:world-db @*test-world) :kind :city))))

  (let [;world (:world-db @*test-world)
        dbs (:dbs @*test-world)
        colsv [[:day]
               [:food/price-float :clothes/price-float :labour/price-float
                :food/price-velocity :clothes/price-velocity :labour/price-velocity
                :food/last-demand :clothes/last-demand :labour/last-demand
                :food/last-supply :clothes/last-supply :labour/last-supply
                :food/last-produced :clothes/last-produced :labour/last-produced
                :food/last-consumed :clothes/last-consumed :labour/last-consumed]
               [:good :base-planning :planning :money]
               [:money-supply]
               [:peep-money]
               [:cloth-money]
               [:food-money]]
        data-fn (fn [world]
                  (->> world
                    ((juxt
                       (fn [w] (lookup-avet w :day nil))
                       (fn [w] (lookup-avet w :kind :city))
                       (fn [w] (lookup-avet w :kind :food-factory))
                       (fn [w] [{:money-supply (pr-str (reduce (fn [v e] (+ v (:money e))) 0 (lookup-avet w :money nil)))}])
                       (fn [w] [{:peep-money (pr-str (reduce (fn [v e] (conj v (:money e))) [] (lookup-avet w :kind :peep)))}])
                       (fn [w] [{:cloth-money (pr-str (reduce (fn [v e] (conj v (:money e))) [] (lookup-avet w :kind :clothes-factory)))}])
                       (fn [w] [{:food-money (pr-str (reduce (fn [v e] (conj v (:money e))) [] (lookup-avet w :kind :food-factory)))}])))
                    (interleave colsv)
                    (into []
                      (comp
                        (partition-all 2)
                        (map (fn [[headers data]]
                               ((apply juxt headers) (first data))))
                        cat))
                    ;(transpose)
                    ;(into [] (map #(into [] cat %)))
                    #_(into [] (map first))
                    #_(apply
                        (juxt
                          (apply juxt [:day])
                          (apply juxt [:food/price-float])))))
        data (map data-fn dbs)]
    (->> data
      (into
        [(into [] cat colsv)])
      (map (partial str/join "\t"))
      ;(take 10)
      (str/join "\n")
      (println)))

  (let [cols [:food/price-float :clothes/price-float :labour/price-float
              :food/price-velocity :clothes/price-velocity :labour/price-velocity
              :food/last-demand :clothes/last-demand :labour/last-demand
              :food/last-supply :clothes/last-supply :labour/last-supply
              :food/last-produced :clothes/last-produced :labour/last-produced
              :food/last-consumed :clothes/last-consumed :labour/last-consumed]
        dbs (:dbs @*test-world)
        ;_ (println (-> dbs first (lookup-avet :kind :city) first d/touch))
        data (mapv
               (fn [world]
                 (-> world
                   (lookup-avet :kind :city)
                   first
                   ((apply juxt cols))))
               dbs)]
    (->> data
      (into [cols])
      (map (partial str/join "\t"))
      ;(take 10)
      (str/join "\n")
      (println))))

(comment
  (do
    (def *test-world (atom {:world-db (reset-world-db) :map-view :default-view}))
    (reset! *sim-broken nil))

  (dotimes [_ 30 #_(* 365 10)]
    (binding [*out* (new java.io.StringWriter)]
      (swap! *test-world tick-world))
    (println (count (lookup-avet (:world-db @*test-world) :kind :city))))

  (let [db (:world-db @*test-world)
        shop
        (fn [db peep-eid]
          (let [{:keys [db/id money planning min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                food-plan (* min-food planning) clothes-plan (* min-clothes planning)
                {food-price :food/price
                 clothes-price :clothes/price
                 food-market :food/market
                 clothes-market :clothes/market
                 home-eid :db/id
                 :as home} (get peep :hometown)
                food-like (like-to-buy money food-price 0.4)
                clothes-like (like-to-buy money clothes-price 0.2)
                food-want (min food-like food-plan) clothes-want (min clothes-like clothes-plan)
                #_#_ #_#_ #_#_ #_#_
                #_#_ #_#_ #_#_ #_#_
                #_#_ #_#_ #_#_ #_#_
                food-cost (* food-want food-price) clothes-cost (* clothes-want clothes-price)
                food-factories (into [] (filter #(= (:good %) :food)) (:_hometown home))
                clothes-factories (into [] (filter #(= (:good %) :clothes)) (:_hometown home))
                food-orders (gen-orders food-want food-price (shuffle food-factories))
                clothes-orders (gen-orders clothes-want clothes-price (shuffle clothes-factories))
                _ (log :debug :home home food-factories)
                _ (log :debug :food-orders food-orders)
                _ (log :debug :clothes-orders clothes-orders)
                food-bought (reduce (fn [val order] (+ val (:buy order))) 0 food-orders)
                clothes-bought (reduce (fn [val order] (+ val (:buy order))) 0 clothes-orders)
                factory-earnings-tx (reduce
                                      (fn [v order]
                                        (let [factory (:from order)
                                              eid (:db/id factory)
                                              money (:money factory)
                                              buying (:buy order)
                                              price (:price order)
                                              sold (:sold factory)
                                              earning (* buying price)]
                                          (log :trace :fac-earn! :eid eid :money money :earning earning :sold sold :buying buying :earned (:earned peep))
                                          (into v [[:db/add eid :money (+ money earning)]
                                                   [:db/add eid :sold (+ sold buying)]
                                                   [:db/add eid :earned (+ (:earned peep) earning)]])))
                                      []
                                      (into food-orders clothes-orders))]
            (println :food-want food-want :food-price food-price :clothes-want clothes-want :clothes-price clothes-price)
            (println {:price food-price :size food-want :side :buys :id id})
            (println {:price clothes-want :size clothes-want :side :buys :id id})
            [[:db/add home-eid :food/market (load-order food-market {:price food-price :size food-want :side :buys :id id})]
             [:db/add home-eid :clothes/market (load-order clothes-market {:price clothes-price :size clothes-want :side :buys :id id})]]
            #_#_#_
            (log :debug peep-eid :shop food-want clothes-want :food-bought food-bought food :clothes-bought clothes-bought clothes (d/touch peep))
            (log :debug :food-like food-like :food-plan food-plan :clothes-like clothes-like :clothes-plan clothes-plan :food/demand (:food/demand home) :clothes/demand (:clothes/demand home))
            (log :debug (mapv d/touch (lookup-avet db :good nil)))
            #_
            (into
              [[:db/add (:db/id home) :food/demand (+ (:food/demand home) food-want)]
               [:db/add (:db/id home) :clothes/demand (+ (:clothes/demand home) clothes-want)]
               [:db/add peep-eid :money (- money food-cost clothes-cost)]
               [:db/add peep-eid :food (+ food food-bought)]
               [:db/add peep-eid :clothes (+ clothes clothes-bought)]]
              factory-earnings-tx)))
        peep-shop-rule
        {:when '[[?e :money ?money]
                 [?e :hometown ?home]
                 [?e :planning ?planning]
                 [?e :food ?food]
                 [?e :clothes ?clothes]
                 [?e :min-food ?min-food]
                 [?e :min-clothes ?min-clothes]
                 [(* ?min-food ?planning) ?food-plan]
                 [(* ?min-clothes ?planning) ?clothes-plan]
                 [(<= ?food ?food-plan) ?not-enough-food]
                 [(<= ?clothes ?clothes-plan) ?not-enough-clothes]
                 [(or ?not-enough-food ?not-enough-clothes)]
                 [?home :food/price ?food-price]
                 [?home :clothes/price ?clothes-price]]
         :then '[[:db.fn/call shop ?e]]
         :call {'shop shop}}

        consume (fn [db peep-eid]
                  (println :consume)
                  (let [{:keys [min-food min-clothes food clothes] :as peep} (d/entity db peep-eid)
                        base-labour 10 ;; reset labour

                        {labour-price :labour/price
                         labour-market :labour/market
                         home-eid :db/id
                         :as home} (get peep :hometown)]
                    [[:db/add home-eid :labour/market (load-order labour-market {:price labour-price :size base-labour :side :sell :id peep-eid})]]))

        peep-consume-rule
        {:when '[[?e :food _]
                 [?e :clothes _]]
         :then '[[:db.fn/call consume ?e]]
         :call {'consume consume}}

        hire (fn [db factory-eid]
               (println :hire)
               (let [{:keys [money sold planning min-labour inventory decay production good] :as factory} (d/entity db factory-eid)
                     _ (log :debug :sold sold)
                     labour-plan (* min-labour planning 10)
                     {home-eid :db/id
                      labour-price :labour/price
                      labour-supply :labour/supply
                      labour-consumed :labour/consumed
                      labour-market :labour/market
                      :as home} (get factory :hometown)
                     labour-like (like-to-buy money labour-price 0.8)
                     labour-want (min labour-like labour-plan)]
                 [[:db/add home-eid :labour/market (load-order labour-market {:price labour-price :size labour-want :side :buys :id factory-eid})]]))

        ;; This models ad-hoc labour, finding work, which could be dice roll based etc, is separate to this, peeps keep doing ad-hoc work while "looking" for a job until they find one, then they stop, unless they switch again.
        hire-rule
        {:when '[[?e :money ?money]
                 [?e :hometown ?home]
                 [?e :inventory ?inventory]
                 [(< ?inventory 100)]]
         :then '[[:db.fn/call hire ?e]]
         :call {'hire hire}}

        ;; TODO: CRAFT IS TOO COMPLEX, SPLIT OUT INTO HIRE, THE MARKET WHICH MANAGES PAYING PEOPLE AND CRAFTING ITSELF, WHICH IS ONLY ABOUT CRAFTING!!
        craft (fn [db factory-eid]
                (println :craft)
                (let [{:keys [money sold planning min-labour labour-bought inventory decay production good] :as factory} (d/entity db factory-eid)
                      produced (condp = good :food (* production labour-bought) :clothes (long (* production (Math/log (+ labour-bought 1)))))
                      decayed-inventory (long (* inventory decay))
                      inventory' (+ decayed-inventory produced)
                      good-price-key (condp = good :food :food/price :clothes :clothes/price)
                      market-key (condp = good :food :food/market :clothes :clothes/market)
                      {home-eid :db/id
                       price good-price-key
                       market market-key
                       :as home} (get factory :hometown)]
                  [[:db/add home-eid market-key (load-order market {:price price :size inventory' :side :sell :id factory-eid})]
                   [:db/add factory-eid :inventory inventory']]))

        craft-rule
        {:when '[[?e :money ?money]
                 [?e :hometown ?home]
                 [?e :inventory ?inventory]
                 [(< ?inventory 100)]]
         :then '[[:db.fn/call craft ?e]]
         :call {'craft craft}}

        match-market
        (fn [db town-eid]
          (println :match-market)
          (let [{food-price :food/price
                 clothes-price :clothes/price
                 food-market :food/market
                 clothes-market :clothes/market
                 labour-market :labour/market
                 :as town} (d/entity db town-eid)]
            ;; TODO: Just have this running once? So we only have one market match, but for now maybe run it twice
            (println :town town)
            (println :food (match-orders food-market))
            (println :clothes (match-orders clothes-market))
            (println :labour (match-orders labour-market))
            (println :matches (:matched (match-orders labour-market)))
            []))

        match-markets-rule
        {:when '[[?e :food/market ?market]]
         :then '[[:db.fn/call match-market ?e]]
         :call {'match-market match-market}}
        db' (infer db
              [peep-shop-rule
               peep-consume-rule
               hire-rule
               craft-rule
               match-markets-rule]
              1)
        city (first (mapv touch (lookup-avet db' :kind :city)))]
    city)

  ,)

