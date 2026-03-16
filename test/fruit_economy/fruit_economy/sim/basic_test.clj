(ns fruit-economy.fruit-economy.sim.basic-test
  (:require [fruit-economy.sim.basic :as basic]
            [fruit-economy.infer.core :refer [infer rewrite]]
            [fruit-economy.sim.market :as market]
            [clojure.test :refer :all]
            [clojure.set :as set]
            [datascript.core :as d]
            [fruit-economy.land :as land]
            [fruit-economy.infer.core :as infer]
            [clojure.data.json :as json]))


(comment
  (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                    {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
        db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}})
             world-data)]
    (is (= (set [[:db/add 4 :place 3]])
          (set (basic/rewrite hunt-rule db))))))

(comment
  (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                    {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
        db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}})
             world-data)
        tx-triples (->> (d/datoms db :eavt)
                     (into [] (map (juxt :e :a :v))))
        eg-db-uri "asami:multi://eg"
        _ (asami.core/delete-database eg-db-uri)
        conn (asami.core/connect eg-db-uri)
        ;;#_#_
        _ @(asami.core/transact conn {:tx-triples tx-triples})
        a-db (asami.core/db conn)]
    (infer/rewrite-asami basic/hunt-rule a-db)))

(deftest hunt-test
  (testing "Testing that hunter will not if there's no better choices"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 0 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's more food where they are"
    (let [world-data [{:food 1 :coord [0 0]} {:food 0 :coord [1 0]} {:food 0 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if there's the same amount of food where they are"
    (let [world-data [{:food 1 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter will move if there's more food elsewhere"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 4 :place 3]])
            (set (basic/rewrite basic/hunt-rule db))))))
  (testing "Testing that hunter won't move if they can't see there's more food elsewhere"
    (let [world-data [{:food 0 :coord [0 0]} {:food 0 :coord [1 0]} {:food 1 :coord [2 0]}
                      {:food 0 :coord [0 1]} {:food 1 :coord [1 1]} {:food 1 :coord [2 1]}
                      {:glyph "🐞", :wealth 0, :vision 1, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/hunt-rule db)))))))

(let [world-data [{:food 2 :coord [0 0]}
                  {:glyph "🐞", :wealth -1, :vision 2, :hunger 3, :place [:coord [0 0]]}]
      db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                 :coord {:db/unique :db.unique/identity}})
           world-data)]
  (basic/rewrite basic/remove-starving-rule db)
  #_(is (= (set [])
          (set (basic/rewrite basic/try-eat-rule db)))))

(deftest try-eat-test
  (testing "Testing that hunter will go hungry if they have no food where they are or wealth and gathering food doesn't make the food negative"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -3]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will go hungry if they have some food where they are, but not enough"
    (let [world-data [{:food 2 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 2 :wealth -1]
                   [:db/add 1 :food 0]])
            (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have enough food where they are"
      (let [world-data [{:food 3 :coord [0 0]}
                        {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
            db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                       :coord {:db/unique :db.unique/identity}})
                 world-data)]
        (is (= (set [[:db/add 2 :wealth 0]
                     [:db/add 1 :food 0]])
              (set (basic/rewrite basic/try-eat-rule db))))))
  (testing "Testing that hunter will not go hungry if they have no food where they are, but have enough wealth"
        (let [world-data [{:food 0 :coord [0 0]}
                          {:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :place [:coord [0 0]]}]
              db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                         :coord {:db/unique :db.unique/identity}})
                   world-data)]
          (is (= (set [[:db/add 2 :wealth 0]
                       [:db/add 1 :food 0]])
                (set (basic/rewrite basic/try-eat-rule db)))))))

(deftest remove-starving-test
  (testing "Testing that not hungry hunter will not be removed"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/remove-starving-rule db))))))
  (testing "Testing that hungry hunter will be removed"
    (let [world-data [{:food 0 :coord [0 0]}
                      {:glyph "🐞", :wealth -1, :vision 2, :hunger 3, :place [:coord [0 0]]}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/retractEntity 2]])
            (set (basic/rewrite basic/remove-starving-rule db)))))))

(deftest grow-food-test
  (testing "Testing food will not grow if at init-food"
    (let [world-data [{:food 1 :coord [0 0] :init-food 1}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [])
            (set (basic/rewrite basic/grow-food-rule db))))))
  (testing "Testing food will grow if not at init-food"
    (let [world-data [{:food 0 :coord [0 0] :init-food 1}]
          db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}})
               world-data)]
      (is (= (set [[:db/add 1 :food 1]])
            (set (basic/rewrite basic/grow-food-rule db))))))
  (testing "Testing food will not grow if greater then init-food"
      (let [world-data [{:food 2 :coord [0 0] :init-food 1}]
            db (d/db-with (d/empty-db {:place {:db/valueType :db.type/ref}
                                       :coord {:db/unique :db.unique/identity}})
                 world-data)]
        (is (= (set [])
              (set (basic/rewrite basic/grow-food-rule db)))))))

(defn db->datoms
  ([db] (db->datoms db []))
  ([db coll]
   (into coll (map (juxt (constantly :db/add) :e :a :v)) (d/datoms db :eavt))))

(defn db-diff [db-before db-after]
  (let [datoms-before (db->datoms db-before #{})
        datoms-after (db->datoms db-after #{})]
    [(set/difference datoms-before datoms-after)
     (set/difference datoms-after datoms-before)]))

(def peep-shop-schema
  {:good {:db/index true}
   :hometown {:db/valueType :db.type/ref}})

(deftest peep-shop-test
  (testing "Testing peeps don't shop when they have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 10] [:db/add 2 :clothes 10]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps do shop when they don't have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 5}
                      {:money 1000 :food 10 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 10}
                      {:money 1000 :food 0 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 20] [:db/add 2 :clothes 10]
                  [:db/add 3 :food 50] [:db/add 3 :clothes 20]
                  [:db/add 4 :food 60] [:db/add 4 :clothes 40]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps who don't have enough money can't shop"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 0 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 5}
                      {:money 0 :food 10 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 10}
                      {:money 0 :food 0 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 0]
                  [:db/add 3 :food 10] [:db/add 3 :clothes 0]
                  [:db/add 4 :food 0] [:db/add 4 :clothes 10]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps don't increase demand when they have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/demand 1] [:db/add 1 :clothes/demand 1]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))
  (testing "Testing peeps do increase demand when they don't have enough food and clothes"
    (let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
                       :clothes/price 1 :clothes/demand 1 :clothes/supply 1
                       :labour/supply 1}
                      {:money 1000 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
          db (d/db-with (d/empty-db peep-shop-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/demand 61] [:db/add 1 :clothes/demand 31]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1))))))))

(testing "Testing when there's no supply peeps can't shop"
  (let [world-data [{:food/price 1 :food/demand 1 :food/supply 0
                     :clothes/price 1 :clothes/demand 1 :clothes/supply 0
                     :labour/supply 0 :db/id -1}
                    {:food/price 1 :food/demand 1 :food/supply 100
                     :clothes/price 1 :clothes/demand 1 :clothes/supply 100
                     :labour/supply 0 :db/id -2}
                    {:money 100 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :hometown -1 :planning 5}
                    {:money 100 :food 10 :clothes 0 :min-food 4 :min-clothes 2 :hometown -1 :planning 10}
                    {:money 100 :food 0 :clothes 10 :min-food 4 :min-clothes 2 :hometown -1 :planning 15}
                    {:good :food :decay 0.9 :production 2 :money 10000 :inventory 20 :min-labour 2 :planning 5 :hometown -1}
                    {:good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 20 :min-labour 2 :planning 5 :hometown -2}]
        db (d/db-with (d/empty-db peep-shop-schema)
             world-data)]
    (basic/infer db [basic/peep-shop-rule] 1)
    #_(is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 0]
                  [:db/add 3 :food 10] [:db/add 3 :clothes 0]
                  [:db/add 4 :food 0] [:db/add 4 :clothes 10]])
            (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1)))))))

(def peep-consume-schema
  {:hometown {:db/valueType :db.type/ref}})

(deftest peep-consume-test
  (testing "Testing food and clothes get consumed"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 2 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 1]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing "Testing if insufficient food or clothes to consume, health lost"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 0 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 2 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 0] [:db/add 2 :health 9]
                  [:db/add 3 :food 0] [:db/add 3 :clothes 1] [:db/add 3 :health 9]
                  [:db/add 4 :food 0] [:db/add 4 :clothes 0] [:db/add 4 :health 9]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing "Testing if sufficient food and clothes to consume, produces enough supply"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:food 2 :clothes 1 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :labour/supply 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1)))))))
  (testing
    #_"Testing if insufficient food or clothes to consume, produce proportionally less supply"
    "Testing if insufficient food or clothes to consume, no change in supply"
    (let [world-data [{:db/id 1 :labour/supply 0}
                      {:db/id 2 :labour/supply 0}
                      {:db/id 3 :labour/supply 0}
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}
                      {:food 0 :clothes 2 :health 10 :min-food 2 :min-clothes 1 :hometown 2}
                      {:food 2 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 3}]
          db (d/db-with (d/empty-db peep-consume-schema)
               world-data)]
      (is (set/subset?
            (set [#_#_#_
                  [:db/add 1 :labour/supply 0]
                  [:db/add 2 :labour/supply 5]
                  [:db/add 3 :labour/supply 5]
                  [:db/add 1 :labour/supply 10]
                  [:db/add 2 :labour/supply 10]
                  [:db/add 3 :labour/supply 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1))))))))



(let [prior-world-data [{:labour/price 1 :labour/demand 1 :labour/supply 20 :labour/consumed 0
                         :food/supply 0 :clothes/supply 0 :food/produced 0 :clothes/produced 0 :db/id -1}
                        {:kind :food-factory :good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown -1}
                        {:kind :clothes-factory :good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown -1}
                        {:kind :peep :money 0 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :labour 10 :hometown -1 :planning 5}]

      world-data [{:labour/price 1 :labour/demand 1 :labour/supply 20 :labour/produced 0 :labour/consumed 0 :labour/market (market/empty-order-book)
                   :food/supply 0 :clothes/supply 0 :food/consumed 0 :clothes/consumed 0 :food/produced 0 :clothes/produced 0 :db/id -1}
                  {:kind :food-factory :good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :labour-bought 0 :hometown -1}
                  {:kind :clothes-factory :good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :labour-bought 0 :hometown -1}
                  {:kind :peep :money 0 :health 10 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :labour 10 :hometown -1 :planning 5 :food/consumed 0 :clothes/consumed 0 :labour/produced 0 :earned 0 :sold 0}]
      db (d/db-with (d/empty-db {:kind {:db/index true}
                                 :hometown {:db/valueType :db.type/ref}})
           world-data)]
  #_(d/q '[:find ?e
           :where (or
                    [?e :food/market _]
                    [?e :clothes/market _]
                    [?e :labour/market _])]
      db)
  (db-diff db (basic/infer db [basic/peep-consume-rule
                               basic/hire-rule
                               basic/match-markets-rule] 1)))




;(deftest craft-test)
(testing "Testing crafting creates stuff when money and labour available"
  (let [world-data [{:labour/price 1 :labour/demand 1 :labour/supply 20 :labour/consumed 0
                     :food/supply 0 :clothes/supply 0 :food/produced 0 :clothes/produced 0 :db/id -1}
                    {:kind :food-factory :good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown -1}
                    {:kind :clothes-factory :good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown -1}
                    {:kind :peep :money 0 :food 0 :clothes 0 :min-food 4 :min-clothes 2 :labour 10 :hometown -1 :planning 5}]
        db (d/db-with (d/empty-db {:kind {:db/index true}
                                   :hometown {:db/valueType :db.type/ref}})
             world-data)]
    (basic/infer db [basic/craft-rule] 1)
    #_[[1 :labour/supply 0]
       [2 :money 9998]
       [2 :inventory 2]]
    #_(is (set/subset?
            (set [[:db/add 2 :food 0] [:db/add 2 :clothes 1]])
            (set (db->datoms (basic/infer db [basic/craft-rule] 1)))))))



(def base-market-data
  {:food/price 100 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0 :food/produced 0 :food/consumed 0
   :clothes/price 200 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0 :clothes/produced 0 :clothes/consumed 0
   :labour/price 300 :labour/price-float 300 :labour/demand 0 :labour/supply 0 :labour/price-velocity 0 :labour/produced 0 :labour/consumed 0})

;; TODO: Update test cases to be inline with the current `update-price-velocity`
(deftest update-prices-test
  (testing "Testing prices don't change when demand equal to supply"
    (let [world-data [(merge base-market-data
                        {:food/price 100 :food/price-float 100 :food/demand 0 :food/supply 0
                         :clothes/price 200 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0
                         :labour/price 300 :labour/price-float 300 :labour/demand 0 :labour/supply 0})]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 100]
                  [:db/add 1 :food/price-float 100]
                  [:db/add 1 :clothes/price 200]
                  [:db/add 1 :clothes/price-float 200]
                  [:db/add 1 :labour/price 300]
                  [:db/add 1 :labour/price-float 300]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing prices increase when demand greater than supply"
    (let [world-data [(merge base-market-data
                        {:food/price 1 :food/price-float 100 :food/demand 1 :food/supply 0
                         :clothes/price 2 :clothes/price-float 200 :clothes/demand 1 :clothes/supply 0
                         :labour/price 3 :labour/price-float 300 :labour/demand 1 :labour/supply 0})
                      (merge base-market-data
                        {:food/price 1 :food/price-float 199 :food/demand 1 :food/supply 0
                         :clothes/price 2 :clothes/price-float 299 :clothes/demand 1 :clothes/supply 0
                         :labour/price 3 :labour/price-float 399 :labour/demand 1 :labour/supply 0})]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 101]
                  [:db/add 1 :clothes/price 2]
                  [:db/add 1 :clothes/price-float 201]
                  [:db/add 1 :labour/price 3]
                  [:db/add 1 :labour/price-float 301]
                  [:db/add 2 :food/price 2]
                  [:db/add 2 :food/price-float 200]
                  [:db/add 2 :clothes/price 3]
                  [:db/add 2 :clothes/price-float 300]
                  [:db/add 2 :labour/price 4]
                  [:db/add 2 :labour/price-float 400]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing prices decrease when demand lower than supply"
    (let [world-data [(merge base-market-data
                        {:food/price 2 :food/price-float 200 :food/demand 0 :food/supply 1
                         :clothes/price 3 :clothes/price-float 300 :clothes/demand 0 :clothes/supply 1
                         :labour/price 4 :labour/price-float 400 :labour/demand 0 :labour/supply 1})
                      (merge base-market-data
                        {:food/price 1 :food/price-float 101 :food/demand 0 :food/supply 1
                         :clothes/price 2 :clothes/price-float 201 :clothes/demand 0 :clothes/supply 1
                         :labour/price 3 :labour/price-float 301 :labour/demand 0 :labour/supply 1})
                      (merge base-market-data
                        {:food/price 2 :food/price-float 200 :food/demand 0 :food/supply 1
                         :clothes/price 3 :clothes/price-float 300 :clothes/demand 0 :clothes/supply 1
                         :labour/price 4 :labour/price-float 400 :labour/demand 0 :labour/supply 1})]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 199]
                  [:db/add 1 :clothes/price 2]
                  [:db/add 1 :clothes/price-float 299]
                  [:db/add 1 :labour/price 3]
                  [:db/add 1 :labour/price-float 399]
                  [:db/add 2 :food/price 1]
                  [:db/add 2 :food/price-float 100]
                  [:db/add 2 :clothes/price 2]
                  [:db/add 2 :clothes/price-float 200]
                  [:db/add 2 :labour/price 3]
                  [:db/add 2 :labour/price-float 300]
                  [:db/add 3 :food/price 1]
                  [:db/add 3 :food/price-float 199]
                  [:db/add 3 :clothes/price 2]
                  [:db/add 3 :clothes/price-float 299]
                  [:db/add 3 :labour/price 3]
                  [:db/add 3 :labour/price-float 399]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1)))))))
  (testing "Testing minimum price is 1"
    (let [world-data [(merge base-market-data
                        {:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 1
                         :clothes/price 1 :clothes/price-float 100 :clothes/demand 0 :clothes/supply 1
                         :labour/price 1 :labour/price-float 100 :labour/demand 0 :labour/supply 1})]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :food/price 1]
                  [:db/add 1 :food/price-float 99]
                  [:db/add 1 :clothes/price 1]
                  [:db/add 1 :clothes/price-float 99]
                  [:db/add 1 :labour/price 1]
                  [:db/add 1 :labour/price-float 99]])
            (set (db->datoms (basic/infer db [basic/update-prices-rule] 1))))))))

;; Test supply equal to total inventory and demand is equal to total planning
(deftest supply-demand-test)
  ;; TODO: Correct this, probably as a generative test
(testing "Testing supply of food and clothes is equal to total inventory"
  (let [world-data [(merge base-market-data
                      {:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/produced 0 :food/consumed 0
                       :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/produced 0 :clothes/consumed 0
                       :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 100 :labour/produced 0 :labour/consumed 0})
                    {:good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}
                    {:good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}]
        db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
             world-data)]
    (basic/infer db [basic/craft-rule] 1)
    #_#_
    (is (set/subset?
          (set [[:db/add 1 :food/supply 4]
                [:db/add 1 :clothes/supply 1]
                [:db/add 2 :inventory 4]
                [:db/add 3 :inventory 1]])
          (set (db->datoms (basic/infer db [basic/craft-rule] 1)))))
    ;; TODO: Fix as we don't take into account decay
    (is (set/subset?
          (set [[:db/add 1 :food/supply 8]
                [:db/add 1 :clothes/supply 2]
                [:db/add 2 :inventory 7]
                [:db/add 3 :inventory 1]])
          (set (db->datoms (basic/infer db [basic/craft-rule] 2))))))
  (testing "Testing supply of labour is equal to total labour"
    (let [world-data [(merge base-market-data
                        {:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/produced 0 :food/consumed 0
                         :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/produced 0 :clothes/consumed 0
                         :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 0 :labour/produced 0 :labour/consumed 0})
                      {:food 0 :clothes 0 :health 10 :min-food 2 :min-clothes 1 :hometown 1}]
          db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
               world-data)]
      (is (set/subset?
            (set [[:db/add 1 :labour/supply 10]
                  [:db/add 1 :labour/produced 10]
                  [:db/add 2 :labour 10]])
            (set (db->datoms (basic/infer db [basic/peep-consume-rule] 1))))))))


;; agreement
;; commitments
;; Trade X food with me [ once | per day for X days | per month for X months ][ , each time you do so, you gain Y ], if you fail to do so, you gain a hook on me.
;; Either party can break the agreement with [ no penalty | a penalty of [ paying X immediately | paying X ] ].


;; party-1 and party-2 agree for X months to form a shared market for [ food | clothes ],
;;   this runs [after, before, instead] of their normal market,
;;   tax earned during trade is [split evenly | [60 40 | 70 30 | 80 20 | 90 10 ] in favour of [ party-1 | party-2 ] | given to [ party-1 | party-2 ] ]

(let [world-data [{:kind :contract
                   :clauses
                   [;; party-1 agrees to give X food per month to party-2, for X months, failing to do so, [ has no penalty as we treat this as a best effort agreement | means a penalty of 10% of any outstanding food owned ].
                    ,]}
                  {:money 10000
                   :food-stockpile 0
                   :clothes-stockpile 0
                   :db/id -1}]

      db (d/db-with (d/empty-db {:governs {:db/valueType :db.type/ref}
                                 :hometown {:db/valueType :db.type/ref}})
           world-data)]
  db)

(testing "")
(let [food-market-1 (-> (market/empty-order-book)
                      (market/load-order {:price 1 :size 10 :side :sell :id 100 :good-kw :inventory}))
      food-market-2 (-> (market/empty-order-book)
                      (market/load-order {:price 1 :size 10 :side :buys :id 200 :good-kw :inventory}))
      world-data [{:food-stockpile-buy? true :money 1 :governs {:db/id -1 :food/market food-market-1 :clothes/market (market/empty-order-book)}}
                  {:hometown -1 :db/id 100 :inventory 0 :money 200}
                  {:food-stockpile-sell? true :sold 0 :earned 0 :money 1 :food-stockpile 100 :governs {:db/id -2 :food/market food-market-2 :clothes/market (market/empty-order-book)}}
                  {:hometown -2 :db/id 200 :inventory 0 :money 200}
                  {:food-stockpile-buy? true :sold 0 :earned 0 :food-stockpile 0
                   :food-stockpile-sell? true :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}
                  #_{:food-stockpile-buy? false :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}
                  #_{:food-stockpile-sell? false :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}
                  #_{:food-stockpile-buy? false
                     :food-stockpile-sell? false :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}
                  #_{:food-stockpile-buy? true
                     :food-stockpile-sell? false :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}
                  #_{:food-stockpile-buy? false
                     :food-stockpile-sell? true :money 1 :governs {:food/market (market/empty-order-book) :clothes/market (market/empty-order-book)}}]
      #_[{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0 :food/produced 0 :food/consumed 0
          :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0 :clothes/produced 0 :clothes/consumed 0
          :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 100 :labour/price-velocity 0 :labour/produced 0 :labour/consumed 0}
         {:good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}
         {:good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}]
      db (d/db-with (d/empty-db {:governs {:db/valueType :db.type/ref}
                                 :hometown {:db/valueType :db.type/ref}})
           world-data)]
  (taoensso.timbre/with-level :trace
    (-> db
      (infer [basic/manage-stockpile-rule basic/match-markets-rule] 1)
      #_(infer basic/decision-rules 1)
      (d/entity 100)
      (d/touch)
      #_(as-> $
          (d/touch
            (d/entity $ (first (d/q
                                 '[:find [?e ...]
                                   :where
                                   [?e :food/market ?m]]
                                 $)))))))
  #_nil)

(testing ""
  (let [world-data [{:food/price 1 :food/price-float 100 :food/demand 0 :food/supply 0 :food/price-velocity 0 :food/produced 0 :food/consumed 0
                     :clothes/price 2 :clothes/price-float 200 :clothes/demand 0 :clothes/supply 0 :clothes/price-velocity 0 :clothes/produced 0 :clothes/consumed 0
                     :labour/price 3 :labour/price-float 300 :labour/demand 0 :labour/supply 100 :labour/price-velocity 0 :labour/produced 0 :labour/consumed 0}
                    {:good :food :decay 0.9 :production 2 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}
                    {:good :clothes :decay 0.98 :production 0.7 :money 10000 :inventory 0 :min-labour 2 :planning 5 :hometown 1}]
        db (d/db-with (d/empty-db {:hometown {:db/valueType :db.type/ref}})
             world-data)]
    (basic/infer db [basic/craft-rule] 1)))

(comment
  (let [schema {:area {:db/index true}
                :land/history {:db/valueType :db.type/ref
                               :db/cardinality :db.cardinality/many}
                :land/resources {:db/valueType :db.type/ref
                                 :db/cardinality :db.cardinality/many}
                :land/units {:db/valueType :db.type/ref
                             :db/cardinality :db.cardinality/many}
                :land/civs {:db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}
                :civ/territory {:db/valueType :db.type/ref
                                :db/cardinality :db.cardinality/many}
                :civ/peeps {:db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}}
        bug-world-size 4 bug-count 2
        coll (basic/gen-bug-world bug-world-size bug-count)
        #_#_
        db (d/db-with (d/empty-db (merge
                                    {:place {:db/valueType :db.type/ref}
                                     :coord {:db/unique :db.unique/identity}}
                                    schema))
             (conj coll
               (->
                 (land/make-land "TEST" 4 3)
                 (land/gen-land))))]
    #_(d/q
        '[:find ?e ?coord ?bfood ?food
          :in $ ?coord
          :where
          [?e :coord ?coord]
          [(get-else $ e :food 0) ?food]
          [(fruit-economy.fruit-economy.sim.basic-test/best-food $ ?coord) ?bfood]]
        db
        [2 1])
    #_(d/q
        '[:find ?e ?vision ?coord ?target ?food ?ce
          :where
          [?e :place ?ce]
          [?e :vision ?vision]
          [?ce :coord ?coord]
          [?ce :food ?food]
          [(fruit-economy.fruit-economy.sim.basic-test/hunt $ ?coord ?vision) ?target]
          [(not= ?target ?coord)]]
        db)
    #_(basic/rewrite basic/hunt-rule db)
    #_(println db)
    #_
    (infer db basic/rules)
    #_(into [] (map (fn [{:keys [loc] :as unit}] (assoc unit :place (basic/coord-q db loc))) (basic/units-q db nil)))
    #_(d/q
        '[:find ?e ?w ?coord ?food
          :where
          [?e :place ?ce]
          [?e :wealth ?w]
          [?ce :coord ?coord]
          [(get-else $ ?ce :food 0) ?food]
          [(> ?food 0)]]
        db)
    db))
