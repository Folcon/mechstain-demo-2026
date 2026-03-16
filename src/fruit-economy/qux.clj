(ns fruit-economy.qux
  (:require [datascript.core :as d]
            [fruit-economy.infer.core :as infer]
            [fruit-economy.sim.basic :as basic]))


(def last-tx-report (atom nil))
(def tx-listeners (atom []))


(def conn (d/create-conn))

(d/transact! conn
  [{:db/id -1 :group/name "Pine Club" :group/sort-by :person/name}
   {:db/id -2 :group/name "Oak Club" :group/sort-by :person/age}])

(let [groups (d/q '[:find [?g ...] :where [?g :group/name _]] @conn)]
  (d/transact!
    conn
    [{:db/id -3 :person/name "Bob" :person/age 30 :person/group (rand-nth groups)}
     {:db/id -4 :person/name "Sally" :person/age 25 :person/group (rand-nth groups)}
     {:db/id -5 :person/name "Lodock" :person/age 45 :person/group (rand-nth groups)}
     {:db/id -6 :person/name "Janis" :person/age 22 :person/group (rand-nth groups)}
     {:db/id -7 :person/name "Angel-Bad" :person/age 14 :person/group (rand-nth groups)}
     {:db/id -8 :person/name "Shomo" :person/age 16 :person/group (rand-nth groups)}
     {:db/id -9 :person/name "Miagianna" :person/age 33 :person/group (rand-nth groups)}
     {:db/id -10 :person/name "Macy" :person/age 4 :person/group (rand-nth groups)}
     {:db/id -11 :person/name "Ojoto" :person/age 20 :person/group (rand-nth groups)}]))


(declare try-tx-listener)

(d/listen! conn :history
  (fn [tx-report]
    (do
      (println (pr-str (:tx-data tx-report)))
      (doall (map (partial try-tx-listener tx-report) @tx-listeners))
      (reset! last-tx-report tx-report))))


(defn tx-item-match? [pattern-item tx-datom-item]
  (cond
    (= pattern-item '_) true
    (coll? pattern-item) (some #{tx-datom-item} pattern-item)
    (fn? pattern-item) (pattern-item tx-datom-item)
    :else (= pattern-item tx-datom-item)))

(defn tx-pattern-match? [pattern tx-datom]
  (cond
    (empty? pattern) true
    (tx-item-match? (first pattern) (first tx-datom))
    (recur (rest pattern) (rest tx-datom))
    :else false))

(defn tx-patterns-match? [patterns tx-datoms]
  (->> (for [p patterns
             d tx-datoms]
         (when (tx-pattern-match? p d) d))
    (filter (fn [b] b))
    first))


(defn query-symbol? [s]
  (and (symbol? s) (= (first (str s)) \?)))

(defn tx-item-match-q? [pattern-item tx-datom-item]
  (cond
    (= pattern-item '_) true
    (query-symbol? pattern-item) {pattern-item tx-datom-item}
    (coll? pattern-item) (some #{tx-datom-item} pattern-item)
    (fn? pattern-item) (pattern-item tx-datom-item)
    :else (= pattern-item tx-datom-item)))

(defn tx-pattern-match-q? [pattern tx-datom]
  (loop [pattern   pattern
         tx-datom tx-datom
         vars      {}]
    (if (empty? pattern)
      vars
      (when-let [v (tx-item-match-q? (first pattern) (first tx-datom))]
        (recur (rest pattern) (rest tx-datom) (if (map? v) (merge v vars) vars))))))


(declare query-unifies?)

(defn tx-patterns-match-q? [db patterns tx-datoms]
  (->> (for [p patterns
             d tx-datoms]
         (if (map? p)
           (when-let [vars (tx-pattern-match-q? (first (keys p)) d)]
             (when (query-unifies? db vars (first (vals p))) {}))
           (tx-pattern-match-q? p d)))
    (remove nil?)
    first))

(defn build-query [vars query]
  (let [ks (keys vars)]
    (vec (concat [:find (vec ks)]
           [:in '$]
           ks
           [:where] query))))

(defn query-unifies? [db vars query]
  (when (not (empty?
               (apply
                  (partial d/q (build-query vars query))
                  (cons db (vals vars)))))
    vars))

(defn tx-match? [db patterns query tx-datoms]
  (when-let [vars (tx-patterns-match-q? db patterns tx-datoms)]
    (if (and query (not (empty? query)))
      (query-unifies? db vars query)
      vars)))


(defn try-tx-listener [tx-report [patterns handler-fn]]
  (when-let [matching-datom (tx-patterns-match? patterns (:tx-data tx-report))]
    (handler-fn matching-datom (:db-after tx-report))))

(defn when-tx [patterns handler-fn]
  (swap! tx-listeners conj [patterns handler-fn]))








(def established-reactions (atom {}))

(defn db-tx
  ([conn patterns] (db-tx conn patterns nil))
  ([conn patterns query]
   (if-let [r (@established-reactions [conn patterns query])]
     r
     (let [new-reaction
           (let [saved-db (atom (d/db conn))
                 #_#_
                 last-tx-report (:last-tx-report (@posh-conns conn))]
             (if (tx-match? @saved-db patterns query
                            (:tx-data @last-tx-report))
               (reset! saved-db (:db-after @last-tx-report))
               @saved-db))]
       (swap! established-reactions merge
              {[conn patterns query] new-reaction})
       new-reaction))))

;; TODO: Write a query and a transact, where the transact doesn't effect the query, but the transact takes updated values from the db.


;; TODO: Submit a query, such that if a transact doesn't update the query, it doesn't change.

(when-tx '[[_ :person/age 21 _]]
  (fn [[e a v] db]
    (println (str "You have come of age, " (:person/name (d/entity db e)) "."))))

;; Reduce time to 0, except when transacted on
(time
  (d/q '[:find [?p ...] :where
         [?p :person/age ?a]]
    @conn))

(defn lookup-pattern [source pattern]
  (println :lookup pattern source)
  (cond
    (satisfies? datascript.db/ISearch source)
    (datascript.query/lookup-pattern-db source pattern)
    :else
    (datascript.query/lookup-pattern-coll source pattern)))

(def mlookup-pattern (memoize lookup-pattern))

(defn -resolve-clause
  ([context clause]
   (-resolve-clause context clause clause))
  ([context clause orig-clause]
   (condp #'datascript.query/looks-like? clause
     '[*] ;; pattern
     (let [source   datascript.query/*implicit-source*
           pattern  (datascript.query/resolve-pattern-lookup-refs source clause)
           relation (lookup-pattern source pattern)
           _ (println :relation relation)]
       (binding [datascript.query/*lookup-attrs* (if (satisfies? datascript.db/IDB source)
                                                   (datascript.query/dynamic-lookup-attrs source pattern)
                                                   datascript.query/*lookup-attrs*)]
         (update context :rels datascript.query/collapse-rels relation)))
     (datascript.query/-resolve-clause context clause orig-clause))))

(with-redefs-fn {#'datascript.query/lookup-pattern (fn [source pattern]
                                                     (lookup-pattern source pattern))
                 #'datascript.query/-resolve-clause -resolve-clause}
  #(time
     (d/q '[:find [?p ...] :where
            [?p :person/age ?a]]
       @conn)))


(let [db (db-tx conn '[[_ :person/age 10]])]
  (d/q '[:find [?p ...] :where
         [?p :person/age ?a]
         [(= ?a 10)]]
    db))



(identity @tx-listeners)

(d/transact! conn
  [{:db/id -11 :person/name "Beo" :person/age 21}])

(:aevt (:db-after @last-tx-report))

(def rules
  [{:when '[[?e :money]
            [(get-else $ ?e :food-stockpile-buy? false) ?food-buy?]
            [(get-else $ ?e :food-stockpile-sell? false) ?food-sell?]
            [(get-else $ ?e :clothes-stockpile-buy? false) ?clothes-buy?]
            [(get-else $ ?e :clothes-stockpile-sell? false) ?clothes-sell?]
            [(or ?food-buy? ?food-sell? ?clothes-buy? ?clothes-sell?)]]}])

(doseq [{:keys [when] :as rule} rules]
  (when-tx when
    (fn [[e a v] db])))

(let [db (d/db-with (d/empty-db {:day {:db/index true}
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
             (drop 8 (:then basic/create-settlement-rule))))]
  (def p-conn (d/conn-from-db db)))

(infer/posh-init! p-conn)

(time
  @(infer/posh-q
     '[:find ?e
       :where
       [?e :money]
       [(get-else $ ?e :food-stockpile-buy? false) ?food-buy?]
       [(get-else $ ?e :food-stockpile-sell? false) ?food-sell?]
       [(get-else $ ?e :clothes-stockpile-buy? false) ?clothes-buy?]
       [(get-else $ ?e :clothes-stockpile-sell? false) ?clothes-sell?]
       [(or ?food-buy? ?food-sell? ?clothes-buy? ?clothes-sell?)]]
     p-conn))

;;(meta p-conn)
(identity posh.clj.datascript/dcfg)

(select-keys @(posh.plugin-base/get-posh-atom posh.clj.datascript/dcfg p-conn) [:graph :ratoms :reactions :cache :filters :retrieve])

