(ns fruit-economy.baz
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [datascript.core :as ds]
            [fruit-economy.infer.core :as infer]
            [fruit-economy.infer.core-test :refer [lookup-avet db->datoms]]
            [fruit-economy.sim.basic :as basic]
            [taoensso.timbre :refer [with-level]]
            [asami.core :as d]
            [clojure.data :as data]))


(let [db (basic/reset-world-db)]
  (def db db))

(def data* (atom nil))

(let [{:keys [ds db conn-db]} @data*
      ds-datoms (db->datoms ds #{})
      db-datoms (db->datoms db #{})]
  (data/diff ds-datoms db-datoms)
  #_
  (vec (sort (set/difference ds-datoms db-datoms)))
  #_
  (vec (sort (set/difference db-datoms ds-datoms)))
  #_db-datoms
  #_(ds/touch (ds/entity ds 10203))
  ;;(infer/infer-posh (ds/conn-from-db ds) [basic/reset-entity-rule] 1)
  ;;(infer/infer db [basic/reset-entity-rule] 1)
  #_nil)

(let [{:keys [ds db conn-db]} @data*
      conn (ds/conn-from-db conn-db)
      inferred-ds (infer/infer conn-db [basic/match-markets-rule basic/update-prices-rule basic/reset-entity-rule] 1)
      inferred-db (infer/infer-conn conn [basic/match-markets-rule basic/update-prices-rule basic/reset-entity-rule] 1)]
  (data/diff inferred-ds @inferred-db
    #_#_
    (->> inferred-ds
      (ds/q
        '[:find ?e
          :where
          [?e :clothes/market]])
      ffirst
      (ds/entity conn-db)
      (ds/touch))
    (->> @inferred-db
      (ds/q
        '[:find ?e
          :where
          [?e :clothes/market]])
      ffirst
      (ds/entity conn-db)
      (ds/touch)))
  #_(->> conn-db
      (ds/q
        '[:find ?e
          :where
          [?e :clothes/market]])
      (ffirst)
      (ds/entity conn-db)
      (ds/touch)
      :clothes/market)
  #_nil)

(let [schema {:place {:db/valueType :db.type/ref}
              :coord {:db/unique :db.unique/identity}}
      world-data [{:init-food 2 :food 2 :coord [0 0]} {:init-food 3 :food 1 :coord [1 0]} {:init-food 2 :food 1 :coord [2 0]}
                  {:glyph "🐞", :wealth 10, :vision 2, :hunger 1, :place [:coord [0 0]]}]
      ;;db (ds/db-with (ds/empty-db schema)
      ;;     world-data)
      ;;db (basic/reset-world-db)
      db (ds/db-with (ds/empty-db {:day {:db/index true}
                                   :money {:db/index true}
                                   :kind {:db/index true}
                                   :good {:db/index true}
                                   :place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}
                                   :settlement/place {:db/valueType :db.type/ref}
                                   :hometown {:db/valueType :db.type/ref}
                                   :governs {:db/valueType :db.type/ref
                                             :db/cardinality :db.cardinality/one}})
           (into [[:db/add -1 :kind :city]
                  [:db/add -1 :settlement/name \A]
                  [:db/add -1 :food/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :clothes/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :labour/market (fruit-economy.sim.market/empty-order-book)]]
             (drop 8 (:then basic/create-settlement-rule))))
      conn (ds/conn-from-db db)
      _ (infer/posh-init! conn)
      rules
      #_[basic/create-settlement-rule]
      (into basic/decision-rules basic/reaction-rules)
      #_[basic/peep-shop-rule
         basic/peep-consume-rule
         basic/adjust-factories-planning-rule
         basic/hire-rule
         basic/craft-rule
         basic/match-markets-rule]
      limit 10]
  (dotimes [_ 2]
    (with-level :warn
      (doseq [rule rules]
        (let [conn-db (ds/db conn)
              _ (println :ds)
              ds (ds/db (infer/infer-posh conn [rule] 1))
              _ (println :db)
              db (infer/infer conn-db [rule] 1)]
          (println := (= ds db))
          (when (and
                  (not= ds db)
                  (not= [nil nil] ((juxt first second) (data/diff ds db))))
            (println :rule rule)
            ;; CHECK HOW OR RULES ARE PARSED IN POSH!!
            (println :fst-snd ((juxt first second) (data/diff ds db)))
            (println :dsdb (pr-str (data/diff
                                     (mapv ds/touch (lookup-avet ds :kind :trade))
                                     (mapv ds/touch (lookup-avet db :kind :trade)))))
            (let [ds-datoms (db->datoms ds #{})
                  db-datoms (db->datoms db #{})]
              (println :APP (set/difference ds-datoms db-datoms) "\n\n")
              (when (and (nil? @data*) #_(< 10 (count (set/difference ds-datoms db-datoms))))
                (reset! data* {:conn-db conn-db
                               :ds ds :db db}))
              (println (data/diff ds-datoms db-datoms))
              (println)
              (println)
              ;;(println :ds-datoms)
              ;;(println (vec (sort ds-datoms)))
              ;;(println)
              ;;(println :db-datoms)
              ;;(println (vec (sort db-datoms)))
              ;;(println)
              ;;(println)
              #_#_#_#_#_#_#_
              (println :diff)
              (println :ds-datoms :db-datoms)
              (println (vec (sort (set/difference ds-datoms db-datoms))))
              (println)
              (println)
              (println :ds-datoms :db-datoms)
              (println (vec (sort (set/difference db-datoms ds-datoms))))))))))
  (select-keys @(posh.plugin-base/get-posh-atom posh.clj.datascript/dcfg conn) [:graph :ratoms :reactions :cache :filters :retrieve])

  #_
  (do (with-level :warn
        (time
          (nth (iterate (fn [conn] (infer/infer-posh conn rules 1)) conn) 100)))
    nil)

  #_
  (do (with-level :warn
        (time
          (nth (iterate (fn [db] (infer/infer db rules 1)) (ds/db conn)) 100)))
    nil)
  #_
  (with-level :warn
    (loop [db db
           n 0]
      (println n)
      (let [db' (infer/infer db rules 1)
            _ (infer/infer-posh conn rules 1)
            ds-datoms (db->datoms db' #{})
            posh-datoms (db->datoms @conn #{})]
        (cond
          (<= limit n)
          db

          (not= ds-datoms posh-datoms)
          [(set/difference ds-datoms posh-datoms)
           (set/difference posh-datoms ds-datoms)]

          :else
          (recur db' (inc n)))))))

(comment
  (identity user/testo)
  (let [dcfg user/dcfg
        fixed-args user/fixed-args]
    (infer/q-analyze dcfg
      '(:results :simple-patterns :results)
      '[:find
        ?e
        ?rem-wealth
        ?e
        ?name
        ?place
        ?empty-order-book
        ?empty-order-book
        ?empty-order-book
        :in
        $
        rand-nth
        ?empty-order-book
        :where
        [?e :wealth ?wealth]
        [(get-else $ ?e :settlement :db/missing) ?s]
        [(= :db/missing ?s)]
        [(>= ?wealth 10)]
        [(- ?wealth 10) ?rem-wealth]
        [?e :place ?place]
        [(rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ") ?letter]
        [(str ?letter) ?name]]
      fixed-args)))

(let [db (ds/db-with (ds/empty-db {:day {:db/index true}
                                   :money {:db/index true}
                                   :kind {:db/index true}
                                   :good {:db/index true}
                                   :place {:db/valueType :db.type/ref}
                                   :coord {:db/unique :db.unique/identity}
                                   :settlement/place {:db/valueType :db.type/ref}
                                   :hometown {:db/valueType :db.type/ref}
                                   :governs {:db/valueType :db.type/ref
                                             :db/cardinality :db.cardinality/one}})
           [])]

     [[:iron {:rate 100}]
      [:coal {:rate 100}]
      [:smith {:rate 5 :needs {:iron 10 :coal 2}}]




      [idx {:label :farmer :tags #{:farmer :grain} :produces 10 :inventory 100}]
      [(inc idx) {:label :miller :tags #{:process}}]
      [:ROSE {:tags #{:plant}}]
      [:LEMON {:tags #{:plant}}]
      [:BASKET {:tags #{:process}}]
      [:GHOST {:tags #{:factory}}]
      [:ROSE :BASKET]
      [:LEMON :BASKET]
      [:BASKET :GHOST]])

(comment
  (let [tx (into [[:db/add -1 :kind :city]
                  [:db/add -1 :settlement/name \A]
                  [:db/add -1 :food/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :clothes/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :labour/market (fruit-economy.sim.market/empty-order-book)]]
             (drop 8 (:then basic/create-settlement-rule)))
        ds-db (ds/db-with (ds/empty-db {:day {:db/index true}
                                        :money {:db/index true}
                                        :kind {:db/index true}
                                        :good {:db/index true}
                                        :place {:db/valueType :db.type/ref}
                                        :coord {:db/unique :db.unique/identity}
                                        :settlement/place {:db/valueType :db.type/ref}
                                        :hometown {:db/valueType :db.type/ref}
                                        :governs {:db/valueType :db.type/ref
                                                  :db/cardinality :db.cardinality/one}})
                tx)
        triples (mapv (juxt :e :a :v) (ds/datoms ds-db :eavt))

        bug-tx (into [] cat (repeat 50 (basic/gen-bug-world basic/bug-world-size basic/bug-count)))

        ds-db (ds/db-with ds-db
                bug-tx)

        eg-db-uri "asami:multi://eg"
        _ (d/delete-database eg-db-uri)
        conn (d/connect eg-db-uri)
        a-db (d/db conn)
        ;;#_#_
        _ @(d/transact conn #_{:tx-data tx} {:tx-triples triples})
        _ @(d/transact conn {:tx-data bug-tx})]

    (=
      (set (time (ds/q
                   '[:find [?e ...]
                     :where
                     [?e :money ?money]
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
                   ds-db)))
      (set
        (time (d/q
                '[:find [?e ...]
                  :where
                  [?e :money ?money]
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
                a-db))))))

