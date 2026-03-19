(ns fruit-economy.foo
  (:require [odoyle.rules :as o]
            [zuko.node :as node]
            [asami.core :as asami]
            [loom.graph :as loom]
            [asami.query :as query]
            [asami.memory :as memory]))


(let [testo-db-uri "asami:multi://testo"
      conn (asami/connect testo-db-uri)
      db (asami/db conn)]
  ;;(loom/nodes (asami/graph db))
  ;;(node/new-node (asami/graph db))
  #_(:pos (asami/graph db))
  #_@(asami/transact conn {:update-fn (fn [g tid]
                                        (println :tid tid :g g)
                                        g)})
  @(asami/transact conn {:tx-data [{:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord-x 0 :coord-y 0}}
                                   {:carrying 6 :vision 2 :hunger 1 :place {:food 2 :max-food 3 :coord-x 0 :coord-y 1}}
                                   {:carrying 4 :vision 1 :hunger 1 :place {:food 2 :max-food 2 :coord-x 1 :coord-y 0}}
                                   {:food 0 :max-food 1 :coord-x 1 :coord-y 1}]})
  #_nil)

(defn sees [coord vision]
  (println :sees coord vision)
  (let [[x y] coord]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] coord)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] coord)]
         [x y])])))

#_#_#_

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

(asami/delete-database "asami:multi://testo")

(defn hunt [db coord-x coord-y v]
  (println :db db :coord coord-x coord-y :vision v)
  [coord-x coord-y])

;; Apply function transform
(let [testo-db-uri "asami:multi://testo"
      conn (asami/connect testo-db-uri)
      db (asami/db conn)
      graph (asami/graph db)
      hunt (fn [db coord-x coord-y v]
             (println :db db :coord coord-x coord-y :vision v)
             [coord-x coord-y])
      max-n (fn [id n id+n-candidates]
              (reduce
                (fn [[id n] [new-id new-n]]
                  (if (> new-n n)
                    [new-id new-n]
                    [id n]))
                [id n]
                id+n-candidates))
      query '[:find ?e ?vision ?ce ?coord-x ?coord-y ?food
              :where
              [?e :place ?ce]
              [?e :vision ?vision]
              [?ce :coord-x ?coord-x]
              [?ce :coord-y ?coord-y]
              [?ce :food ?food]
              #_#_
              [(vector ?coord-x ?coord-y) ?coord]
              [(fruit-economy.foo/sees ?coord ?vision) ?sees]
              #_
              [(fruit-economy.foo/hunt ?sees ?coord-x ?coord-y ?vision) ?target]
              #_[(hunt $ ?coord-x ?coord-y ?vision) ?target]]]
  (binding [asami.query/*override-restrictions* true]
    (let [inputs (#'asami/graphs-of [db])]
      (for [[ent-id vision loc-id coord-x coord-y food] (query/query-entry query memory/empty-graph inputs false)
            :let [seen (sees [coord-x coord-y] vision)
                  _ (println :seen seen)
                  alternative-locations (query/query-entry
                                          [:find '?e '?food
                                           :where
                                           (conj (into () (map (fn [[x y]] (list 'and ['?e :coord-x x] ['?e :coord-y y] ['?e :food '?food])) seen)) 'or)]
                                          memory/empty-graph inputs false)]]
        ;; if best = current, no need to change
        [:ent-id ent-id :best (max-n loc-id food alternative-locations) :loc-id loc-id :food food :alts alternative-locations]
        #_(query/query-entry
            '[:find
              :where
              [?e :place ?ce]
              [?e :vision ?vision]
              [?ce :coord-x ?coord-x]
              [?ce :coord-y ?coord-y]
              [?ce :food ?food]]
            memory/empty-graph inputs false)))
    #_(let [inputs [db] plan? false
            {:keys [find all in with where]} (query/parse query)
            [inputs options] (if (seq in)
                               [(take (count in) inputs) (drop (count in) inputs)]
                               [[(first inputs)] (rest inputs)])
            options (-> (apply hash-map options) (assoc :query-plan plan?))
            [bindings default-graph] (query/create-bindings in inputs)
            graph (or default-graph memory/empty-graph)]
        bindings)
    ;; I can do this within an update-fn I think?
    #_(query/query-entry query memory/empty-graph (#'asami/graphs-of [db]) false)
    #_(asami/q query db)))

[:ent-id :a/node-66075 :loc-id :a/node-66076 :food 2 :alts ([:a/node-66078 2] [:a/node-66080 2])]

;; Doesn't work!!!
(let [eg-db-uri "asami:multi://eg"
      conn (asami/connect eg-db-uri)
      _ @(asami/transact conn {:tx-triples
                               [[:a/node-65706 :carrying 0]
                                [:a/node-65706 :vision 1]
                                [:a/node-65706 :hunger 2]
                                [:a/node-65707 :food 2]]})
      db (asami/db conn)
      pred (fn [db e a]
             (println :db db e a)
             (println (asami/entity db e))
             (= a (:age (asami/entity db e))))]
  (binding [asami.query/*override-restrictions* true]
   (asami/q '[:find ?e
              :in $ ?pred
              :where [?e :food ?a]
              [(?pred $ ?e 10)]]
     db pred)))

(reduce
  (fn [[id n] [new-id new-n]]
    (if (> new-n n)
      [new-id new-n]
      [id n]))
  [:a/node-66076 2]
  '([:a/node-66078 2] [:a/node-66080 2]))

(defn ent-xf [db] (map (fn [node-id] (asami/entity db node-id))))

(let [testo-db-uri "asami:multi://testo"
      conn (asami/connect testo-db-uri)
      db (asami/db conn)]
  (into []
    (comp (ent-xf db)
      (map (fn [{:keys [vision place]}] (sees [(:coord-x place) (:coord-y place)] vision))))
    (asami/q
      '[:find [?e ...]
        :where
        [?e :place]]
      db)))

(let [testo-db-uri "asami:multi://testoa"
      conn (asami/connect testo-db-uri)
      _ @(asami/transact conn {:tx-triples
                               [[:a/node-65706 :carrying 0]
                                [:a/node-65706 :vision 1]
                                [:a/node-65706 :hunger 2]
                                [:a/node-65707 :food 2]
                                [:a/node-65707 :max-food 4]
                                [:a/node-65707 :coord [0 0]]
                                [:a/node-65706 :a/owns :a/node-65707]
                                [:a/node-65706 :place :a/node-65707]
                                [:a/node-65706 :db/ident :a/node-65706]
                                [:a/node-65706 :a/entity true]
                                [:a/node-65710 :carrying 6]
                                [:a/node-65710 :vision 2]
                                [:a/node-65710 :hunger 1]
                                [:a/node-65711 :food 2]
                                [:a/node-65711 :max-food 3]
                                [:a/node-65711 :coord [0 1]]
                                [:a/node-65710 :a/owns :a/node-65711]
                                [:a/node-65710 :place :a/node-65711]
                                [:a/node-65710 :db/ident :a/node-65710]
                                [:a/node-65710 :a/entity true]
                                [:a/node-65712 :carrying 4]
                                [:a/node-65712 :vision 1]
                                [:a/node-65712 :hunger 1]
                                [:a/node-65713 :food 2]
                                [:a/node-65713 :max-food 2]
                                [:a/node-65713 :coord [1 0]]
                                [:a/node-65712 :a/owns :a/node-65713]
                                [:a/node-65712 :place :a/node-65713]
                                [:a/node-65712 :db/ident :a/node-65712]
                                [:a/node-65712 :a/entity true]
                                [:a/node-65714 :food 0]
                                [:a/node-65714 :max-food 1]
                                [:a/node-65714 :coord [1 1]]
                                [:a/node-65714 :db/ident :a/node-65714]
                                [:a/node-65714 :a/entity true]]
                               #_[{:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [0 0]}}
                                  {:carrying 6 :vision 2 :hunger 1 :place {:food 2 :max-food 3 :coord-x 0 :coord-y 1}}
                                  {:carrying 4 :vision 1 :hunger 1 :place {:food 2 :max-food 2 :coord-x 1 :coord-y 0}}
                                  {:food 0 :max-food 1 :coord-x 1 :coord-y 1}]})
      db (asami/db conn)]
  (asami/q
    '[:find [?e ...]
      :where
      [?e :coord [0 0]]]
    db)
  #_(into []
      (comp (ent-xf db)
        (map (fn [{:keys [vision place]}] (sees [(:coord-x place) (:coord-y place)] vision))))
      (asami/q
        '[:find [?e ...]
          :where
          [?e :place]]
        db)))

(comment
  (let [testo-db-uri "asami:multi://example"
        conn (asami/connect testo-db-uri)
        _ @(asami/transact conn {:tx-data [{:coord [0 0]}]})
        db (asami/db conn)]
    (asami/q
      '[:find [?e ...]
        :where
        [?e :coord [0 0]]]
      db))

 #_=> ()

 (let [testo-db-uri "asami:multi://example"
       conn (asami/connect testo-db-uri)
       db (asami/db conn)
       new-node (zuko.node/new-node (asami/graph db))
       _ @(asami/transact conn {:tx-triples [[new-node :coord [0 0]]]})
       db (asami/db conn)]
   (asami/q
     '[:find [?e ...]
       :where
       [?e :coord [0 0]]]
     db))

 #_=> (:a/node-65763))
