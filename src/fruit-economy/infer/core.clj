(ns fruit-economy.infer.core
  (:require [clojure.walk :as walk]
            [datascript.core :as ds]
            [posh.clj.datascript :as pd]
            [posh.lib.util :as util]
            [asami.core :as asami]
            [asami.query]))


(defn lookup-pattern [source pattern]
  (println :lookup pattern)
  (cond
    (satisfies? datascript.db/ISearch source)
    (datascript.query/lookup-pattern-db source pattern)
    :else
    (datascript.query/lookup-pattern-coll source pattern)))

(def mlookup-pattern (memoize lookup-pattern))

(defn rewrite
  "Tries to match rule against db
  Returns tx-data or empty list if rule does not match"
  [{:keys [when then args call]} db]
  (let [ds-var? (fn [sym] (= (first (name sym)) \?))
        syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)
        results
        (apply ds/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))
        #_(with-redefs-fn {#'datascript.query/lookup-pattern (fn [source pattern]
                                                               (lookup-pattern source pattern))}
            #(apply ds/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args)))]
    (for [match results tx then]
      (let [swaps (merge
                    (zipmap syms match)
                    call)
            f (fn [x] (if (coll? x) x (get swaps x x)))]
        (walk/postwalk f tx)))))

(defn infer
  "Returns a new db with all inferred facts and a list of tx-data.
  Stops when no more rules apply or after 100 iterations"
  ([db rules] (infer db rules 100))
  ([db rules max-iter]
   (loop [db db max-iter max-iter]
     (let [tx-data (for [rule rules :let [#_#_ _ (println "RULE:\n" max-iter rule)] tx (rewrite rule db) :let [#_#_ _ (println tx)]] tx)]
       (cond
         (empty? tx-data) db
         (zero? max-iter) db
         :else (recur (ds/db-with db tx-data) (dec max-iter)))))))

(defn infer-conn
  ([conn rules] (infer-conn conn rules 100))
  ([conn rules max-iter]
   (loop [conn conn max-iter max-iter]
     (let [tx-data (for [rule rules tx (rewrite rule (ds/db conn))] tx)]
       (cond
         (empty? tx-data) conn
         (zero? max-iter) conn
         :else (recur (do (ds/transact! conn tx-data) conn) (dec max-iter)))))))

(defn rewrite-asami
  "Tries to match rule against db
  Returns tx-data or empty list if rule does not match"
  [{:keys [when then args call]} db]
  (binding [asami.query/*override-restrictions* true]
    (let [ds-var? (fn [sym] (= (first (name sym)) \?))
          syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)
          results (apply asami/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))]
      (for [match results tx then]
        (let [swaps (merge
                      (zipmap syms match)
                      call)
              f (fn [x] (if (coll? x) x (get swaps x x)))]
          (walk/postwalk f tx))))))

(defn infer-asami
  "Returns a new db with all inferred facts and a list of tx-data.
  Stops when no more rules apply or after 100 iterations"
  ([conn rules] (infer-asami conn rules 100))
  ([conn rules max-iter]
   (loop [conn conn max-iter max-iter]
     (let [tx-data (for [rule rules :let [#_#_ _ (println "RULE:\n" max-iter rule)] tx (rewrite-asami rule conn) :let [#_#_ _ (println tx)]] tx)]
       (cond
         (empty? tx-data) conn
         (zero? max-iter) conn
         :else (recur (do (asami/transact conn tx-data) conn) (dec max-iter)))))))


(def posh-init! pd/posh!)
(def posh-q pd/q)
(def posh-transact! pd/transact!)

(defn rewrite-posh [{:keys [when then args call]} conn]
  (let [ds-var? (fn [sym] (= (first (name sym)) \?))
        syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)
        ->vec-query (into [:find]
                      cat
                      [syms
                       [:in '$]
                       (keys args)
                       [:where] when])
        #_#_
        _ (println :->vec-query ->vec-query :vals (vals args))
        results (apply posh-q ->vec-query conn (vals args))]
    (for [match @results tx then]
      (let [swaps (merge
                    (zipmap syms match)
                    call)
            f (fn [x] (if (coll? x) x (get swaps x x)))]
        (walk/postwalk f tx)))))

(defn infer-posh
  ([conn rules] (infer-posh conn rules 100))
  ([conn rules max-iter]
   (loop [conn conn max-iter max-iter]
     (let [tx-data (for [rule rules tx (rewrite-posh rule conn)] tx)]
       (cond
         (empty? tx-data) conn
         (zero? max-iter) conn
         :else (recur (do (posh-transact! conn tx-data) conn) (dec max-iter)))))))



;;(defn q-analyze [dcfg retrieve query args]
;;  ;;(println retrieve query args)
;;  (let [qm           (merge
;;                      {:in '[$]}
;;                      (posh.lib.q-analyze/query-to-map query))
;;        where        (posh.lib.q-analyze/normalize-all-eavs (vec (:where qm)))
;;        eavs         (posh.lib.q-analyze/get-eavs where)
;;        vars         (vec (posh.lib.q-analyze/get-all-vars eavs #_query #_where #_eavs))
;;        ;;_ (println :where where)
;;        ;;_ (println :eavs eavs)
;;        ;;_ (println :where-vars (vec (posh.lib.q-analyze/get-all-vars where)))
;;        ;;_ (println :eavs-vars (vec (posh.lib.q-analyze/get-all-vars eavs)))
;;        ;;_ (println :query-vars (posh.lib.q-analyze/get-all-vars query))
;;        newqm        (merge qm {:find vars :where where})
;;        ;; This doesn't seem to be getting used anymore
;;        ;;newq         (qm-to-query newqm)
;;        dbvarmap     (posh.lib.q-analyze/make-dbarg-map (:in qm) args)
;;        fixed-args   (->> (zipmap (:in qm) args)
;;                          (map (fn [[sym arg]]
;;                                 (or (:db (get dbvarmap sym)) arg))))
;;        r            (apply (partial (:q dcfg) newqm) fixed-args)
;;        lookup-ref-patterns
;;        (->> args
;;             ;; Would be nice to check by the schema as well, to make sure this is actually a identity attribute
;;             (filter (every-pred vector? (comp keyword? first) (comp (partial = 2) count)))
;;             (map (fn [[a v]] ['$ '_ a v])))]
;;    ;;(println :newqm newqm)
;;    ;;(println :q {:find (vec (:find qm))
;;    ;;             :in [[vars '...]]})
;;    ;;(println :r (vec r))
;;    (merge
;;     (when (some #{:datoms :datoms-t} retrieve)
;;       (let [datoms (posh.lib.q-analyze/split-datoms (posh.lib.q-analyze/create-q-datoms r eavs vars))]
;;         (merge
;;          (when (some #{:datoms} retrieve)
;;            {:datoms
;;             (->> datoms
;;                  (map (fn [[db-sym db-datoms]]
;;                         {(:db-id (dbvarmap db-sym))
;;                          db-datoms}))
;;                  (apply merge))})
;;          (when (some #{:datoms-t} retrieve)
;;            {:datoms-t
;;             (->> datoms
;;               (map (fn [[db-sym db-datoms]]
;;                      (let [db (dbvarmap db-sym)]
;;                        {(:db-id db)
;;                         (util/t-for-datoms (:q dcfg) (:db db) db-datoms)})))
;;               (apply merge))}))))
;;     (when (some #{:results} retrieve)
;;       {:results
;;        ((:q dcfg) {:find (vec (:find qm))
;;                    :in [[vars '...]]}
;;         (vec r))})
;;     (when (some #{:patterns :filter-patterns :simple-patterns} retrieve)
;;       (let
;;           [in-vars      (posh.lib.q-analyze/get-input-sets (:q dcfg) (:in qm) args)
;;            eavs-ins    (map (fn [[db & eav]]
;;                               (vec
;;                                (cons db
;;                                      (map
;;                                       (fn [var-name]
;;                                         (if-let [var-value (in-vars var-name)]
;;                                           (posh.lib.q-analyze/resolve-any-idents (:entid dcfg)
;;                                                               (:db (get dbvarmap db))
;;                                                               (:schema (get dbvarmap db))
;;                                                               where
;;                                                               var-name
;;                                                               var-value)
;;                                           var-name))
;;                                       eav))))
;;                             (concat lookup-ref-patterns eavs))
;;            qvar-count   (posh.lib.q-analyze/count-qvars eavs-ins)
;;            linked-qvars (set (remove nil? (map (fn [[k v]] (if (> v 1) k)) qvar-count)))
;;            rvars        (zipmap
;;                          vars
;;                          (posh.lib.q-analyze/stack-vectors r))
;;            prepped-eavs (clojure.walk/postwalk
;;                          #(if (and (posh.lib.q-analyze/qvar? %) (not (linked-qvars %))) '_ %)
;;                          eavs-ins)]
;;         (merge
;;          (when (some #{:simple-patterns} retrieve)
;;            {:patterns
;;             (posh.lib.q-analyze/patterns-from-eavs dbvarmap rvars
;;                                 (clojure.walk/postwalk #(if (posh.lib.q-analyze/qvar? %) '_ %)
;;                                                        eavs-ins))})
;;          (when (some #{:patterns} retrieve)
;;            {:patterns (posh.lib.q-analyze/patterns-from-eavs dbvarmap rvars prepped-eavs)
;;             :linked   linked-qvars})
;;          (when (some #{:filter-patterns} retrieve)
;;            {:filter-patterns (posh.lib.q-analyze/filter-patterns-from-eavs dbvarmap rvars prepped-eavs)})))))))
;;
;;(alter-var-root #'posh.lib.q-analyze/q-analyze (fn [_] q-analyze))

(comment
  (require '[fruit-economy.sim.basic :as basic])
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
             (into [[:db/add -1 :kind :city]
                    [:db/add -1 :settlement/name \A]
                    [:db/add -1 :food/market (fruit-economy.sim.market/empty-order-book)]
                    [:db/add -1 :clothes/market (fruit-economy.sim.market/empty-order-book)]
                    [:db/add -1 :labour/market (fruit-economy.sim.market/empty-order-book)]]
               (drop 8 (:then basic/create-settlement-rule))))]
    (time
      (dotimes [_ 10]
        (infer db
          [basic/peep-shop-rule
           basic/peep-consume-rule
           #_basic/adjust-factories-planning-rule
           #_basic/hire-rule
           #_basic/craft-rule
           #_basic/manage-stockpile-rule
           #_basic/infrastructure-hire-rule
           #_basic/match-markets-rule
           #_basic/infrastructure-fix-rule
           #_basic/infrastructure-decay-rule
           #_basic/update-prices-rule
           #_basic/reset-entity-rule
           #_basic/state-tax-rule] 1)))
    nil))