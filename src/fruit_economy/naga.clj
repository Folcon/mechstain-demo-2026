(ns fruit-economy.naga
  (:require [naga.schema.structs :as structs :refer [new-rule]]
            [naga.rules :as r :refer [r regen-rewrite]]
            [naga.engine :as e]
            [naga.store :as store]
            [naga.storage.asami.core :as mem]
            [asami.core :as asami]
            [zuko.node :as node]
            [asami-loom.index]
            [asami-loom.label]
            [asami-loom.multi-graph]
            [loom.graph :as loom]))

(let [rules [(r "shared-parent" [?b :parent ?c] :- [?a :sibling ?b] [?a :parent ?c])
             (r "sibling->brother" [?a :brother ?b] :- [?a :sibling ?b] [?b :gender :male])
             (r "uncle" [?a :uncle ?c] :- [?a :parent ?b] [?b :brother ?c])
             ;; (r "siblings" [?a :sibling ?b] :- [?a :parent ?p] [?b :parent ?p] (not= ?a ?b))
             (r "male-father" [?f :gender :male] :- [?a :father ?f])
             (r "parent-father" [?a :parent ?f] :- [?a :father ?f])]
      axioms
      [[:fred :sibling :barney]
       [:fred :parent :mary]
       [:mary :sibling :george]
       [:george :gender :male]]

      dburi "asami:mem://family"
      _ (asami/delete-database dburi)
      _ (asami/create-database dburi)
      conn (asami/connect dburi)
      _ @(asami/transact conn {:tx-triples axioms})

      program (r/create-program rules [])
      [store results] (e/run conn program)
      unk (asami/q '[:find ?n ?u :where [?n :uncle ?u]] (asami/db conn))]
  [[program store results]
   unk])

(r "shared-parent" [?b :parent ?c] :- [?a :sibling ?b] [?a :parent ?c])

(let [rules [#_#_#_#_#_
             (r "shared-parent" [?b :parent ?c] :- [?a :sibling ?b] [?a :parent ?c])
             (r "sibling->brother" [?a :brother ?b] :- [?a :sibling ?b] [?b :gender :male])
             (r "uncle" [?a :uncle ?c] :- [?a :parent ?b] [?b :brother ?c])
             ;; (r "siblings" [?a :sibling ?b] :- [?a :parent ?p] [?b :parent ?p] (not= ?a ?b))
             (r "male-father" [?f :gender :male] :- [?a :father ?f])
             (r "parent-father" [?a :parent ?f] :- [?a :father ?f])
             (r "grow" [?f :grow ?mxf] :- [?e :food ?f] [?e :max-food ?mxf] [?e :food (max ?f ?mf)])]

      axioms
      [[0 :food 0] [0 :max-food 4] [0 :coord [0 0]]
       [1 :food 0] [1 :max-food 3] [1 :coord [0 1]]
       [2 :food 0] [2 :max-food 2] [2 :coord [1 0]]
       [3 :food 0] [3 :max-food 1] [3 :coord [1 1]]

       [100 :wealth 0] [100 :vision 4] [100 :hunger 4] [100 :place 1]
       [101 :wealth 6] [101 :vision 4] [101 :hunger 2] [101 :place 2]
       [102 :wealth 4] [102 :vision 3] [102 :hunger 2] [102 :place 3]]


      dburi "asami:mem://bug"
      _ (asami/delete-database dburi)
      _ (asami/create-database dburi)
      conn (asami/connect dburi)
      _ @(asami/transact conn {:tx-triples axioms})

      program (r/create-program rules)
      [store results] (e/run conn program)

      unk (asami/q '[:find ?n ?u :where [?n :food ?u]] (asami/db conn))]
  unk)

(require '[naga.lang.pabu :as pabu])

(pabu/read-str
  "north(A, B) :- cell(X, Y), cell(X, NY), NY is Y - 1.")


(defn limited-inc [x mx] (min (inc x) mx))

(defn sees [x y vision]
  (println :sees x y vision)
  (let [coord [x y]]
    (into []
      cat
      [(for [x (range (- x vision) (inc (+ x vision)))
             :when (not= [x y] coord)]
         [x y])
       (for [y (range (- y vision) (inc (+ y vision)))
             :when (not= [x y] coord)]
         [x y])])))

(defn hunt [sees]
  (println :hunt sees))

(comment
  (binding [asami.query/*override-restrictions* true]
    (let [testo-db-uri "asami:multi://testo"
          _ (asami/delete-database testo-db-uri)
          conn (asami/connect testo-db-uri)
          _ (asami/transact conn {:tx-data [{:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [0 0]}}
                                            {:carrying 6 :vision 2 :hunger 1 :place {:food 2 :max-food 3 :coord [0 1]}}
                                            {:carrying 4 :vision 1 :hunger 1 :place {:food 2 :max-food 2 :coord [1 0]}}
                                            {:food 0 :max-food 1 :coord [1 1]}]})
          db (asami/db conn)
          ;#_#_
          [store stats] (->> [(r "grow" [?e :food' ?f'] :- [?e :max-food ?mxf] [?e :food ?f] [(not= ?f ?mxf)] [(fruit-economy.naga/limited-inc ?f ?mxf) ?f'])
                              (r "hunt" [?e :place' ?p'] :- [?e :place ?pe] [?pe :coord ?ce] [?ce :a/first ?cf] [?ce :a/rest ?cer] [?cer :a/first ?crf] [?e :vision ?v] [(fruit-economy.naga/sees ?cf ?crf ?v) ?sees] [(fruit-economy.naga/hunt ?sees) ?hunt])]
                          (r/create-program)
                          (e/run conn))]
      (asami/graph db)
      (println :stats stats)
      #_(mapv identity (asami/q '[:find ?n ?u :where [?n :food ?u]] (asami/db conn))))))

(comment
  (let [testo-db-uri "asami:multi://testo"
        conn (asami/connect testo-db-uri)
        db (asami/db conn)]
    (:pos (asami/graph db))
    #_@(asami/transact conn {:update-fn (fn [g tid]
                                          (println :tid tid :g g)
                                          g)})
    #_nil))

