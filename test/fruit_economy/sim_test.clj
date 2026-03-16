(ns fruit-economy.sim-test
  (:require [clojure.test :refer :all]
            [fruit-economy.land :as land]
            [fruit-economy.language :as lang]
            [fruit-economy.economy :as economy]
            [fruit-economy.unit :as unit]
            [fruit-economy.civ :as civ]
            [fruit-economy.db.core :as db]
            [fruit-economy.data.core :as data]
            [fruit-economy.game :as game]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

;(defn try-spawn-new-civs [{::land/keys [width height] :as land-data} n-attempts]
;  (reduce
;    (fn [land _n]
;      (let [x (rand-int width)
;            y (rand-int height)
;            target (get-in land [::land/terrain y x])
;            occupied? (get-in land [::land/area->civ-name [x y]])]
;        (println :try-spawn-civ _n target :occupied? occupied?)
;        (if (and (not= target :ocean)
;              (not occupied?))
;          (spawn-civ land x y {})
;          land)))
;    land-data
;    (range n-attempts)))
;
;(defn spawn-civ [{::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} x y {:keys [parent expand?]}]
;  (if (seq civ-letters)
;    (let [symbol (first civ-letters)
;          civ-name (make-word lang)
;          biome (get-in terrain [y x])
;          new-peep {::id curr-civ-id
;                    ::name civ-name
;                    :name (str "Peep " (inc (rand-int 1000)))
;                    :kind :peep
;                    :glyph "🧑"
;                    :area [x y]
;                    :on-tick peep-on-tick
;                    :decisions [:claim :develop :gather :grow]}
;          new-civ (make-civ curr-civ-id civ-name symbol [x y] name biome parent [new-peep])]
;      (-> land-data
;        (log-history (str "Spawning new civ at " x " " y " on " biome))
;        (assoc-in [::land/area->civ-name [x y]] civ-name)
;        (update ::land/civ-name->civ assoc civ-name new-civ)
;        (update :land/civs (fnil conj []) new-civ)
;        (update ::land/civ-letters disj symbol)
;        (update ::land/curr-civ-id inc)
;        (economy/init-civ-economy new-civ)
;        (cond->
;          expand?
;          (as-> $ (civ-actions/expand-territory $ (get-in $ [::civ-name->civ civ-name]))))))
;    (-> land-data
;      (log-history (str "Tried to spawn new civ at " x " " y " ran out of letters")))))


(comment
  (def l
    (let [width  6
          height 4
          land (-> (land/make-land "World" width height)
                 (land/gen-land)
                 (land/populate 50 #_100)
                 (unit/spawn-units 10)
                 (economy/add-resources)
                 (civ/try-spawn-new-civs 10))]
      land))
  (def d (db/db-bulk-insert (db/init-db) [l]))

  (data/entity (db/db-bulk-insert
                 (db/db-bulk-insert
                   (db/init-db)
                   [{:db/ident :history :land/history [{:history/entry "1"} {:history/entry "2"}]}])
                 [{:db/ident :history :land/history [{:history/entry "4"} {:history/entry "6"}]}])
    '[* {:land/history [*]}]
    [:db/ident :history])

  (db/db-bulk-insert (db/init-db) [(select-keys l [:land/history])])

  (let [world-db d]
    (into [] (map :history) (sort-by :id (db/q '[:find [(pull ?v [[:db/id :as :id] [:history/entry :as :history]]) ...] :where [?e :land/history ?v]] world-db))))

  (let [world-db d
        area [3 2]]
    (db/q '[:find (pull ?c [* {:civ/territory [*] :civ/peeps [*]}]) . :where [?e :area ?a] [?c :civ/territory ?e] :in $ ?a] world-db area))

  (let [world-db d
        n-attempts 6
        world-db' (game/on-tick world-db)
        touch-civ (fn [id] (data/entity world-db '[* {:civ/territory [*] :civ/peeps [*]}] id))
        [world-name width height civ-letters terrain lang curr-civ-id] (db/q '[:find [?n ?w ?h ?civ-letters ?t ?lang ?curr-civ-id]
                                                                               :where
                                                                               [?e :fruit-economy.land/name ?n]
                                                                               [?e :fruit-economy.land/width ?w]
                                                                               [?e :fruit-economy.land/height ?h]
                                                                               [?e :fruit-economy.land/curr-civ-id ?curr-civ-id]
                                                                               [?e :fruit-economy.land/civ-letters ?civ-letters]
                                                                               [?e :fruit-economy.land/terrain ?t]
                                                                               [?e :fruit-economy.land/lang ?lang]]
                                                                         world-db)
        existing-claims (into #{} (map :area) (data/land-claims world-db))
        candidates (into [] (distinct) (repeatedly n-attempts #(vector (rand-int width) (rand-int height))))]
    (reduce
      (fn [v [x y]]
        (let [target (get-in terrain [y x])
              occupied? (contains? existing-claims target)]
          (println :try-spawn-civ target :occupied? occupied?)
          (if (and (not= target :ocean)
                (not occupied?))
            (into v
              (let [symbol (first civ-letters)
                    civ-name (lang/make-word lang)
                    biome (get-in terrain [y x])
                    new-peep {::id curr-civ-id
                              ::name civ-name
                              :name (str "Peep " (inc (rand-int 1000)))
                              :kind :peep
                              :glyph "🧑"
                              :area [x y]
                              :on-tick #'civ/peep-on-tick
                              :decisions [:claim :develop :gather :grow]}
                    new-civ (civ/make-civ curr-civ-id civ-name symbol [x y] world-name biome nil [new-peep])]
                (if (seq civ-letters)
                  (conj (data/log-history (str "Spawning new civ at " x " " y " on " biome))
                    new-civ)
                  [(data/log-history (str "Tried to spawn new civ at " x " " y " ran out of letters"))])))
            v)
          ,))
      []
      candidates)
    #_(let [x (rand-int width)
            y (rand-int height)
            target (get-in terrain [y x])]
        target)
    ;(civ/try-spawn-new-civs (data/land-data world-db) 1)
    #_(data/land-data world-db)
    #_(tap> [#_(data/land-data world-db)
             #_(data/land-data world-db')
             (mapv touch-civ (data/ordered-civs world-db))
             (mapv touch-civ (data/ordered-civs world-db'))])))