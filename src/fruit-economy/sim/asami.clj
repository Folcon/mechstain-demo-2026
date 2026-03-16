(ns fruit-economy.sim.asami
  (:require [clojure.walk :as walk]
            [asami.core :as d]
            [asami.memory :as mem]
            [asami.graph :as gr :refer [graph-add graph-delete graph-diff resolve-triple count-triple]]
            [asami-loom.index]
            [asami-loom.label]
            [loom.io :as loom-io]
            [fruit-economy.infer.core :refer [rewrite infer]]
            [zuko.node :as node]
            [asami.storage :as storage]
            [asami.entities :as entities]
            [asami.entities.reader :as reader]
            [hiccup.core :refer [html]]
            [loom.graph :as loom]
            [loom.alg :refer [topsort]])
  (:import [java.util.concurrent CompletableFuture]
           [java.util.function Supplier]
           [guru.nidi.graphviz.model Factory Link MutableNode MutableGraph]
           [guru.nidi.graphviz.engine Format Graphviz Engine]
           [guru.nidi.graphviz.attribute Color Label]))

(def db-uri "asami:mem://dbname")
(d/create-database db-uri)

(def db-conn (d/connect db-uri))

(def transact d/transact)

(def conn d/connect)
(def db d/db)
(def graph d/graph)
(def create-database d/create-database)
(def delete-database d/delete-database)
(def connect d/connect)
(def entity d/entity)
(def ident->entity reader/ident->entity)
(def empty-graph mem/empty-graph)
(def q d/q)
(def new-node node/new-node)
(def export-data d/export-data)
(def export-str d/export-str)

(defn edge-label
  [g s d]
  (str (d/q '[:find ?edge . :in $ ?a ?b :where (or [?a ?edge ?b] [?b ?edge ?a])] g s d)))

(defn id->str [n id]
  (cond
    id (str id)
    (and (keyword? n) (= (namespace n) "a")) (str ":" (name n))
    :default (str n)))

(defn node-label
  [g n]
  (let [id (d/q [:find '?id '. :where [n :db/ident '?id]] g)]
    (id->str n id)))

(defn asami->graph [graph {:keys [node-label-fn node-color-fn edge-label-fn edge-color-fn] :as opts}]
  (as-> {:nodes {} :edges []} $
    (reduce
      (fn [g node-id]
        (println :node node-id (ident->entity graph node-id))
        (let [node-id' (str node-id)
              new-node (merge {:node-id node-id'}
                         (when-let [color (and node-color-fn (node-color-fn graph node-id))]
                           {:color color})
                         (when-let [label (and node-label-fn (node-label-fn graph node-id))]
                           {:label label}))]
          (update g :nodes assoc node-id' new-node)))
      $
      (loom/nodes graph))

    (reduce
      (fn [g [src dest]]
        (let [new-edge (merge {:src (str src) :dest (str dest)}
                         (when-let [color (and edge-color-fn (edge-color-fn graph src dest))]
                           {:color color})
                         (when-let [label (and edge-label-fn (edge-label-fn graph src dest))]
                           {:label label}))]
          (update g :edges conj new-edge)))
      $
      (loom/edges graph))))

(defn asami->svg [graph]
  (let [g (-> (Factory/mutGraph)
            (.setDirected true))
        data {:graph g :nodes {} :edges {}}]
    (as-> data $
      (reduce
        (fn [g node-id]
          (let [{:keys [id color label] :as node-label} (node-label graph node-id)
                _ (println :node node-label :node-id node-id)
                node-id (str node-id)
                label (str node-label)
                new-node (cond-> (Factory/mutNode ^String node-id)
                           color
                           (.add (Color/named (name color)))
                           label
                           (.add (Label/html (html label))))]
            (-> g
              (update :nodes assoc node-id new-node)
              (update :graph #(.add % [new-node])))))
        $
        (loom/nodes graph))

      (reduce
        (fn [{:keys [nodes] :as g} [src dest]]
          (let [{:keys [id color label] :as edge} (edge-label graph src dest)
                _ (println :edge edge :src src :dest dest)
                src-id (str src)
                dest-id (str dest)
                src-node (get nodes src-id)
                dest-node (get nodes dest-id)
                _ (println (pr-str :src-id src-id :dest-id dest-id :src-node src-node :dest-node dest-node :nodes (keys nodes)))
                link (.addLink ^MutableNode src-node [(cond-> (Link/to dest-node)
                                                        edge
                                                        (.with ^Link (Label/html (html edge))))])]
            g))
        $
        (loom/edges graph))

      (-> (Graphviz/fromGraph ^MutableGraph (:graph $))
        (.engine Engine/CIRCO)
        (.width 1200)
        (.height 800)
        (.render Format/SVG)
        (.toString)))))

(defn graph->svg [{:keys [nodes edges] :as graph}]
  (let [g (-> (Factory/mutGraph)
            (.setDirected true))
        data {:graph g :nodes {} :edges {}}]
    (as-> data $
      (reduce-kv
        (fn [g node-id {:keys [color label] :as node}]
          (let [new-node (cond-> (Factory/mutNode ^String node-id)
                           color
                           (.add (Color/named (name color)))
                           label
                           (.add (Label/html (html label))))]
            (-> g
              (update :nodes assoc node-id new-node)
              (update :graph #(.add % [new-node])))))
        $
        nodes)

      (reduce
        (fn [{:keys [nodes] :as g} {:keys [src dest label color]}]
          (let [src-node (get nodes src)
                dest-node (get nodes dest)
                _ (.addLink ^MutableNode src-node [(cond-> (Link/to dest-node)
                                                     label
                                                     (.with ^Link (Label/html (html label))))])]
            g))
        $
        edges)

      (-> (Graphviz/fromGraph ^MutableGraph (:graph $))
        (.engine Engine/CIRCO)
        (.width 1200)
        (.height 800)
        (.render Format/SVG)
        (.toString)))))

(defn view [graph]
  (-> graph
    (asami->graph {:node-label-fn (fn [g n] (pr-str (or (ident->entity g n) (node-label g n)))) :edge-label-fn edge-label})
    (graph->svg))
  #_(asami->svg graph)
  #_(loom-io/dot-str graph {:edge-label (partial edge-label graph) :node-label (partial node-label graph)}))

(defonce econ-graph (atom empty-graph))

#_(defn add-simplices-indexes [state simplices]
    (reduce-kv
      (fn [st idx simplex]
        (reduce-kv
          (fn [st' idx' {:keys [input output]}]
            (-> st'
              (update-in [:in->out idx idx' input] (fnil conj #{}) output)
              (update-in [:out->in idx idx' output] (fnil conj #{}) input)))
          st
          simplex))
      state
      simplices))

(comment
  (d/delete-database db-uri)

  (let [fast-chicken {:input {:name :corn}
                      :edge {:name :fast-farmer :likelihood 1 :in 3}
                      :output {:name :chicken-broiler :quantity 3}}
        avg-chicken {:input {:name :corn}
                     :edge {:name :avg-farmer :likelihood 3 :in 5}
                     :output {:name :chicken-broiler :quantity 4}}
        slow-chicken {:input {:name :corn}
                      :edge {:name :slow-farmer :likelihood 1 :in 8}
                      :output {:name :chicken-broiler :quantity 7}}

        good-butcher {:edge {:name :butcher :likelihood 1}
                      :input {:name :chicken-carcass :quantity 1}
                      :output {:name :chicken-meat :quantity 3}}
        avg-butcher {:edge {:name :butcher :likelihood 3}
                     :input {:name :chicken-carcass :quantity 1}
                     :output {:name :chicken-meat :quantity 2}}
        bad-butcher {:edge {:name :butcher :likelihood 1}
                     :input {:name :chicken-carcass :quantity 1}
                     :output {:name :chicken-meat :quantity 1}}
        data [fast-chicken
              avg-chicken
              slow-chicken
              good-butcher
              avg-butcher
              bad-butcher]]
    (->> (reduce
           (fn [g {:keys [input edge output]}]
             (let []
               (graph-add g input edge output)))
           mem/empty-graph
           data)
      (reset! econ-graph)))

  (let [out-degree (fn [g node]
                     (count (disj (apply clojure.set/union (map (comp set keys) (vals ((:spo g) node)))) nil)))
        edge-fn (fn [g s d] (ffirst (get-in (:osp g) [d s])))
        graph @econ-graph
        nodes (loom/nodes graph)
        raw-materials (filterv (fn [n] (zero? (loom/in-degree graph n))) nodes)
        simplices (into [] (comp
                             (filter (fn [n] (> (out-degree graph n) 1)))
                             (map (fn [n] (into [] (map (fn [[_ [target edge]]] {:input n :output target :edge (merge (edge-fn graph n target) edge)})) (loom/out-edges graph n))))
                             cat) nodes)
        state {:raw-materials raw-materials :simplices simplices}
        init-goods 1000
        ;; set raw materials to init-goods
        graph' (reduce
                 (fn [g material]
                   (graph-add g {:goods init-goods} {:name :source} material))
                 graph
                 raw-materials)
        ;; Here we're just doing a start weighted to the size of the simplex, so each option is evenly weighted.
        ;;   We could also set our own weights, with the assumption that they add to one.
        relaxed-start (mapv (fn [simplex] (let [size (count simplex)] (vec (repeat size (/ 1 size))))) simplices)]
    ;; Now to compute flow through the network
    #_(topsort graph')
    #_[(reduce
         (fn [g material]
           (let [{:keys [in->out out->in]} state
                 in->out-paths (reduce-kv
                                 (fn [paths k i->o]
                                   (println k i->o)
                                   (into paths
                                     (reduce-kv
                                       (fn [paths' k' v']
                                         (println :inner k' (get v' material) v')
                                         (if-let [hit (get v' material)]
                                           (conj paths' [k k' material hit])
                                           paths'))
                                       paths
                                       i->o)))
                                 []
                                 in->out)]
             (println material simplices)
             in->out-paths
             #_(graph-add g {:goods init-goods} {:name :source} material)))
         graph
         raw-materials)
       (update simplices 0 dissoc 0)]
    #_(:spo graph)
    #_(disj (set (keys (apply clojure.set/union (vals ((:spo graph) {:name :chicken-carcass, :quantity 1}))))))
    #_(count (disj (apply clojure.set/union (vals ((:spo graph) {:name :chicken-carcass, :quantity 1}))) nil))
    #_(loom/out-degree graph {:name :chicken-carcass, :quantity 1})
    #_(mapv (fn [n] (loom/out-degree graph n)) nodes)
    #_{:in-edges (mapv (partial loom/in-edges graph) nodes)
       :in-degree (mapv (partial loom/in-degree graph) nodes)
       :out-edges (mapv (partial loom/out-edges graph) nodes)
       :out-degree (mapv (partial out-degree graph) nodes)})

  (let [_ (delete-database peep-db-uri)
        _ (create-database peep-db-uri)
        peep-conn (connect peep-db-uri)
        db (db peep-conn)
        things [#_{:name :butcher
                   :input [{:name :chicken-carcass}]}
                {:input {:name :chicken-carcass :quantity 1}
                 :output {1 {:name :chicken-meat :quantity 2 :likelihood 1}}}]]
    (-> @(transact peep-conn {:tx-data things})
      #_:tx-data
      :db-after
      #_((fn [db] (mapv (partial entity db) (d/q '[:find [?m ...] :where [?m :input _]] db))))
      graph
      #_(asami->graph {:node-label-fn (fn [g n] (pr-str (or (ident->entity g n) (node-label g n)))) :edge-label-fn edge-label})
      view))

  (let [things
        #_
        [[:db.fn/call (fn [& args]
                        (println (pr-str args)))]]
        ;#_
        [{:name :state
          :day 0}
         {:name :wheat-farm
          :kind :farm
          :farmer {:crop [:wheat :peas]}
          :fields [{:kind :field :crop :empty}
                   {:kind :field :crop :empty}
                   {:kind :field :crop :empty}
                   {:kind :field :crop :empty}
                   {:kind :field :crop :empty}]}]
        #_
        (into [{:name :state
                :day 0}]
          (comp cat cat)
          [(for [idx (range 1 10 10)
                 sidx (range 2 10)
                 :let [idx' (- idx)
                       sidx' (- idx' sidx)]]
             [{:db/id idx'
               :name :wheat-farm
               :kind :farm}
              {:db/id sidx'
               :kind :field}
              [:db/add idx' :fields sidx']])])]
    (-> @(d/transact db-conn {:tx-data things})
      :tx-data))

  (let []
    @(d/transact db-conn {:update-fn (fn [db id]
                                       (println :keys (keys db))
                                       (println :vals (vals db))
                                       (println :id id)
                                       db)}))

  (let [db (d/db db-conn)
        rewrite (fn [{:keys [when then args call]} db]
                  (let [ds-var? (fn [sym] (= (first (name sym)) \?))
                        syms (for [row then el row :when (and (symbol? el) (ds-var? el))] el)
                        results (apply d/q {:find syms :in (cons '$ (keys args)) :where when} db (vals args))]
                    (for [match results tx then]
                      (let [swaps (merge
                                    (zipmap syms match)
                                    call)
                            f (fn [x] (if (coll? x) x (get swaps x x)))]
                        (walk/postwalk f tx)))))]
    (rewrite
      #_{:when '[[?e :day ?day]
                 [(inc ?day) ?next-day]]
         :then '[[:db/add ?e :day ?next-day]]}
      {:when '[[?e :crop :empty]
               [?e :kind :field]
               [?farm :fields ?e]
               [?farm :farmer ?f]
               [?f :crop ?c]]
       :then '[[:db/add ?e :day ?next-day]]}
      db))

  (let [db (d/db db-conn)]
    (-> (d/q '[:find ?c
               :where
               [?e :crop :empty]
               [?e :kind :field]
               [?v :a/contains ?e]
               [?farm :fields ?v]
               [?farm :farmer ?f]
               [?f :crop ?cv]
               [?cv :a/contains ?c]] db)
      ,))

  (let [things
        [{:name :wheat-farm
          :kind :farm
          :fields [{:kind :field :crop :empty}]}]]
    (-> @(d/transact db-conn {:tx-data things})
      :tx-data))

  (let [db (d/db db-conn)]
    (d/q '[:find ?kind
           :where
           [?e :crop :empty]
           [?e :kind :field]
           [?v :a/contains ?e]
           [?farm :fields ?v]
           [?farm :kind ?kind]] db))

  (let [db (d/db db-conn)]
    (d/q '[:find ?e ?a ?e2
           :where
           [?e :crop :empty]
           [?e2 :kind :farm]
           ;;[?e2 ?a* ?e]
           [?e2 ?a+ ?e]]
      (d/db db-conn)))

  (let [db (d/db db-conn)
        g (d/graph db)
        inbound-edges (fn [node]
                        (asami.graph/resolve-triple g '?node '?edge node))
        outbound-edges (fn [node]
                         (asami.graph/resolve-triple g node '?edge '?node))

        node :a/node-58407]
    ;;(outbound-edges node)
    (d/q '[:find ?node ?edge
           :in $ ?n
           :where [?node ?edge ?n]]
      db node)))

;; Use `transact-update` to produce updates to the graph via:
;;   graph-add / graph-delete))
;;   Look at `transact-data*`
;; Or use tx-triples

;; Look at asami.reader @ into-multimap for good example of transducer usage.

(defn transact-async
  ;; returns a deref'able object that derefs to:
  ;; {:db-before DatabaseType
  ;;  :db-after DatabaseType
  ;;  :tx-data [datom/DatomType]
  ;;  :tempids {s/Any s/Any}}
  "Updates a database.
   connection: The connection to the database to be updated.
   tx-info: This is either a seq of items to be transacted, or a map.
            If this is a map, then a :tx-data value will contain the same type of seq that tx-info may have.
            Each item to be transacted is one of:
            - vector of the form: [:db/add entity attribute value] - creates a datom
            - vector of the form: [:db/retract entity attribute value] - removes a datom
            - map: an entity to be inserted/updated.
            Alternatively, a map may have a :tx-triples key. If so, then this is a seq of 3 element vectors.
            Each vector in a :tx-triples seq will contain the raw values for [entity attribute value]
            :executor An optional value in the tx-info containing an executor to be used to run the CompletableFuture
            :input-limit contains an optional maximum number of statements to insert (approx)
  Entities and assertions may have attributes that are keywords with a trailing ' character.
  When these appear an existing attribute without that character will be replaced. This only occurs for the top level
  entity, and is not applied to attributes appearing in nested structures.
  Entities can be assigned a :db/id value. If this is a negative number, then it is considered a temporary value and
  will be mapped to a system-allocated ID. Other entities can reference such an entity using that ID.
  Entities can be provided a :db/ident value of any type. This will be considered unique, and can be used to identify
  entities for updates in subsequent transactions.

  Returns a future/delay object that will hold a map containing the following:
  :db-before    database value before the transaction
  :db-after     database value after the transaction
  :tx-data      sequence of datoms produced by the transaction
  :tempids      mapping of the temporary IDs in entities to the allocated nodes"
  [{:keys [name state] :as connection} tx-info]

  ;; Detached databases need to be reattached when transacted into
  (d/check-attachment connection)

  ;; destructure tx-info, if it can be destructured
  (let [{:keys [tx-data tx-triples executor update-fn input-limit]} (if (map? tx-info) tx-info {})
        _ (println :tx-data tx-data :tx-triples tx-triples :update-fn update-fn)
        op (if update-fn
             (fn []
               (let [[db-before db-after] (storage/transact-update connection update-fn)]
                 {:db-before db-before
                  :db-after db-after}))
             (fn []
               (let [current-db (storage/db connection)
                     ;; single maps should not be passed in, but if they are then wrap them
                     seq-wrapper (fn [x] (if (map? x) [x] x))
                     ;; volatiles to capture data for the user
                     ;; This is to avoid passing parameters to functions that users may want to call directly
                     ;; and especially to avoid the difficulty of asking users to of return multiple structures
                     vtempids (volatile! {}) ;; volatile to capture the tempid map from built-triples
                     generated-data (volatile! [[] []]) ;; volatile to capture the asserted and retracted data in a transaction
                     [db-before db-after] (if tx-triples
                                            ;; simple assertion of triples
                                            (storage/transact-data connection generated-data (seq-wrapper tx-triples) nil)
                                            ;; a seq of statements and/or entities
                                            ;; convert these to assertions/retractions and send to transaction
                                            ;; also, capture tempids that are generated during conversion
                                            (storage/transact-data connection
                                                                   generated-data
                                                                   (fn [graph]
                                                                     ;; building triples returns a tuple of assertions, retractions, tempids
                                                                     (let [[_ _ tempids :as result]
                                                                           (entities/build-triples graph
                                                                                                   (seq-wrapper (or tx-data tx-info))
                                                                                                   input-limit)]
                                                                       (vreset! vtempids tempids)
                                                                       result))))
                     ;; pull out the info captured during the transaction
                     [triples retracts] (deref generated-data)]
                 {:db-before db-before
                  :db-after db-after
                  :tx-data (concat retracts triples)
                  :tempids @vtempids})))]
    (CompletableFuture/supplyAsync (reify Supplier (get [_] (op)))
      (or executor clojure.lang.Agent/soloExecutor))))

(alter-var-root #'d/transact-async (fn [_] transact-async))

(def peep-db-uri "asami:mem://peep-beliefs")
(d/create-database peep-db-uri)

(comment
  ;; Build model of peep knowledge
  (let [_ (d/delete-database peep-db-uri)
        _ (d/create-database peep-db-uri)
        peep-conn (d/connect peep-db-uri)
        db (d/db peep-conn)
        things [#_{:name :butcher
                   :input [{:name :chicken-carcass}]}
                {:input {:name :chicken-carcass :quantity 1}
                 :output {1 {:name :chicken-meat :quantity 2 :likelihood 1}}}]]
    (-> @(d/transact peep-conn {:tx-data things})
      #_:tx-data
      :db-after
      ((fn [db] (mapv (partial d/entity db) (d/q '[:find [?m ...] :where [?m :input _]] db)))))))

(def testo-db-uri "asami:mem://testo")

(comment
  (let [testo-db-uri "asami:mem://testo"
        _ (d/delete-database testo-db-uri)
        _ (d/create-database testo-db-uri)
        testo-conn (d/connect testo-db-uri)
        db (d/db testo-conn)
        new-node (node/new-node (d/graph db))
        tx-triples (entities/build-triples (d/graph db) [[:db/add new-node :pear {:fish 12}]])]
    @(d/transact testo-conn #_{:tx-data [{:pear {:fish 12}}]} {:tx-triples (first tx-triples)})))
