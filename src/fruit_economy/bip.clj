(ns fruit-economy.bip
  (:require [datascript.core :as ds]
            [fruit-economy.sim.basic :as basic]))


(let [tx [[:db/add -1 :kind :city]
          [:db/add -1 :settlement/name \A]]
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
              tx)]
  (ds/q
    {:find '[[?e ...]]
     :where
     '[[?e :kind ?c]]}
    ds-db))

(->> (datascript.parser/parse-query
       '[:find ?e
         :where
         [?e :kind ?c]
         [?e :settlement/name ?c]])
  :qwhere)

(require '[naga.lang.pabu])
(require '[naga.engine])

(-> (naga.lang.pabu/read-str
      "ancestor(X, Y) :- parent(X, Y).
       ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)."))

(->> (naga.rules/r "shared-parent" [?b :parent ?c] :- [?a :sibling ?b] [?a :parent ?c])
  (vector)
  (naga.rules/create-program)
  :rules
  (naga.engine/initialize-rules)
  #_(naga.engine/run {:type :memory})
  vals
  (reduce
    naga.queue/add
    (naga.queue/new-queue :salience :name))
  (naga.queue/head)
  :status
  first
  naga.engine/extract-dirty-pattern)

#_
(q {:find [(list 'count fvar) '.]
    :with rvars
    :where [pattern]} db)

(let [tx [[:db/add -1 :kind :city]
          [:db/add -1 :settlement/name \A]]
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
              tx)]
  (def ds-db ds-db))

(ds/q
  {:find '[[?e ...]]
   :where
   '[[?e :kind ?c]]}
  ds-db)

(def ds-db-bug (basic/reset-world-db))


(let [ds-db ds-db-bug
      refs (reduce-kv (fn [m k v] (cond-> m (= (:db/valueType v) :db.type/ref) (conj k))) #{} (:schema ds-db-bug))
      rule basic/hunt-rule
      {:keys [when then args call]} rule
      ds-var? (fn [sym] (= (first (name sym)) \?))
      syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)]
  #_(reduce (fn [v pattern] (if (= (count pattern) 3) (let [attr (nth pattern 1)] (if (contains? refs attr) (conj v ['_ attr '_]) (conj v ['_ attr]))) v)) #{} when)
  (-> {:find syms :in (cons '$ (keys args)) :where when}
    #_(datascript.parser/parse-query)
    ((fn [query] (apply ds/q query ds-db (vals args))))))

