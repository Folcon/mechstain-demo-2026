(ns fruit-economy.economy
  (:require [clojure.walk :as walk]
            [ubergraph.core :as uber]
            [ubergraph.alg :as uber.alg]
            [fruit-economy.land :as land]
            [fruit-economy.graph :as graph]))


(defn dec-or-dissoc [m k]
  (let [v (dec (get m k))]
    (if (zero? v)
      (dissoc m k)
      (assoc m k v))))

(defn make-civ-node [{:fruit-economy.civ/keys [name tint] :as civ} sources]
  {:id (str name) :color "blue"
   :label (into [:table {:border 0}
                 [:tr [:td {:border 1} (str name)]]]
            (map (fn [[k v]] [:tr [:td (:name k)] [:td {:border 1} v]]))
            sources)})

(defn assign-source->civ [{::land/keys [economy area->civ-name area->resources] :as land-data} {:fruit-economy.civ/keys [name] :as _civ} area]
  (let [existing-owner (area->civ-name area)
        resource (area->resources area)]
    (cond-> (land/log-history land-data (str (when existing-owner (str existing-owner " relinquishes claim on and ")) name " lays claim to a " (:name resource) " at " area))
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc resource)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source name resource] (fnil inc 0)))))

(defn init-civ-economy [{::land/keys [area->resources] :as land-data} {:fruit-economy.civ/keys [name] :as civ}]
  (let [;; stop recording sources on the node directly
        civ-node (make-civ-node civ [])]
    (-> land-data
      ;; Add the civ node to the graph
      (update-in [::land/economy :ubergraph] graph/add-node-with-attrs [name civ-node])

      ;; Mark down new claims, should really use assign-source->civ,
      ;;   but it doesn't work with the graph yet
      (as-> $
        (reduce
          (fn [land area]
            (if-let [resource (area->resources area)]
              (-> (land/log-history land (str name " lays claim to a " (:name resource) " at " area))
                (update-in [::land/economy :civ-name->source name resource] (fnil inc 0))
                (update-in [::land/economy :ubergraph] graph/add-directed-edge name (:name resource) {:label (str "at " area)}))
              land))
          $
          (mapv :area (:civ/territory civ)))))))

(defn add-resources [{::land/keys [area->resources] :as land-data}]
  (reduce
    (fn [land {:keys [name kind] :as resource}]
      (let [node-kind (->> (get land/kind->category kind)
                        clojure.core/name
                        (str "exists-")
                        keyword)
            resource-node {:id name :ref resource
                           :kind node-kind
                           :label (str name "\n" node-kind)}]
        (-> land
          (update-in [::land/economy :ubergraph] graph/add-node-with-attrs [name resource-node]))))
    land-data
    (distinct (vals area->resources))))

(comment
  (require '[fruit-economy.civ :as civ])
  (do
    (swap! fruit-economy.core/*state assoc :world w :cell 200)
    nil)

  (def w
    (let [width 3 height 4
          world (-> (land/populate (land/gen-land (land/make-land "World" width height)) 100)
                  (civ/try-spawn-new-civs 10))]
      world))

  (let [land-data w
        {::land/keys [economy civ-name->civ area->resources area->civ-name] :as land-data} land-data
        civ-name "Civ )+3"
        civ  (civ-name->civ civ-name)
        area [1 0]
        existing-owner (area->civ-name area)
        resource (area->resources area)]
    (cond-> land-data
      ;; remove from existing owner
      existing-owner
      (update-in [::land/economy :civ-name->source existing-owner] dec-or-dissoc resource)

      ;; add to new owner
      :always
      (update-in [::land/economy :civ-name->source civ-name resource] (fnil inc 0))

      :always
      ::land/economy)
    ;nil
    #_(-> (reduce
            (fn [land area]
              (let [resource (area->resources area)]
                (println area resource)
                (-> land
                  (update-in [::land/economy :civ-name->source civ-name resource] (fnil inc 0))
                  #_(update-in [::land/economy :area->source area] resource))))
            land-data
            (:fruit-economy.civ/territory civ))
        ::land/economy))

  (let [world #_w (:world @fruit-economy.core/*state)]
    ((juxt
       ::land/area->resources
       (comp
         #(map (juxt :fruit-economy.civ/name :fruit-economy.civ/territory) %)
         vals ::land/civ-name->civ)
       ::land/economy)
     world))

  ,)

(defn ->svg [{:keys [ubergraph nodes edges] :as economy}]
  (cond-> economy

    ;; contains nodes or edges. so make ubergraph
    (or nodes edges)
    (graph/make)

    ;; contains ubergraph, so grab it
    :ubergraph
    (get :ubergraph)

    :always
    (graph/->svg)))

;; categories of resources
;; :bush :tree :flower :herb :shroom :nut :fruit
(def processes [:chop :boil :magic :heat :slice])

(defn apply-rules [graph rules]
  (let [rules' (shuffle rules)]
    (reduce
      (fn [g {:keys [match alter] :as _rule}]
        (let [result (reduce
                       (fn [v match-node]
                         (println match-node (graph/find-nodes g match-node))
                         (if-let [matches (graph/find-nodes g match-node)]
                           (conj v matches)
                           (reduced [])))
                       []
                       match)]
          (when (every? seq result)
            (println "HIT " _rule))
          (if (every? seq result)
            (apply alter g result)
            g)))
      graph
      rules')))

(defn id+kind+tag->label [{:keys [id kind tag] :as _node}] (str id "<br />" kind (when tag (str " " tag))))

(defn generate-rules []
  (let [categories [:bush :tree :flower :herb :shroom :nut :fruit]]
    (into []
      (comp
        (map
          (fn [category-kw]
            (let [category-str (name category-kw)
                  exists-kw (keyword (str "exists-" category-str))
                  unused-kw (keyword (str "unused-" category-str))
                  exists-process-kw (keyword (str "exists-" category-str "-process"))
                  exists-processed-kw (keyword (str "exists-" category-str "-processed"))
                  processed-kw (keyword (str category-str "-processed"))
                  exists-good-kw (keyword (str "exists-" category-str "-good"))
                  good-kw (keyword (str category-str "-good"))]
              [;; innovation-token =>
               ;;   (exists-category-process|exists-category-good)
               {:match [[{:kind :innovation-token}]]
                :alter (fn [graph innovation-tokens]
                         (let [innovation-token (first innovation-tokens)
                               token-id (re-find #"[0-9]+" (:id innovation-token))
                               category-kw (rand-nth categories)
                               category-str (name category-kw)
                               innovation (rand-nth ["process" "good"])
                               innovation-str (str category-str "-" innovation)
                               exists-innovation-str (str "exists-" innovation-str)
                               exists-innovation-kw (keyword exists-innovation-str)
                               innovation-node-id (str innovation-str "-" token-id)]
                           (-> graph
                             ;; remove the old token, we might want to keep it in the future if they're created by civ's
                             ;;   and are linked to them, we should probably use numbers as id's so that we don't need to rename nodes
                             (graph/remove-node (:id innovation-token))
                             (graph/add-node-with-attrs [innovation-node-id {:id innovation-node-id :kind exists-innovation-kw :color "purple"}])
                             (graph/update-labels {:nodes [{:id innovation-node-id :label-fn id+kind+tag->label}]}))))}

               ;; source + exists-category =>
               ;;   unused-category +
               ;;   [source unused-category]
               {:match [[{:kind :source}]
                        [{:kind exists-kw}]]
                :alter (fn [graph sources exists-categories]
                         (let [source (rand-nth sources)
                               exists-category (rand-nth exists-categories)]
                           (println "new-rule:\n" (str source " " exists-category " => " unused-kw " " [(:id source) (:id exists-category)]))
                           (-> graph
                             (assoc-in [:attrs (:id exists-category) :kind] unused-kw)
                             (graph/add-directed-edge (:id source) (:id exists-category))
                             (graph/update-labels {:nodes [{:id (:id exists-category) :label-fn id+kind+tag->label}]}))))}

               ;; unused-category + exists-category-process =>
               ;;   category + category-processed + processor +
               ;;   [category processor] [processor category-processed]
               {:match [[{:kind unused-kw}]
                        [{:kind exists-process-kw}]]
                :alter (fn [graph unused-categories exists-category-processes]
                         (let [unused-category (rand-nth unused-categories)
                               exists-category-process (rand-nth exists-category-processes)
                               processor (rand-nth processes)
                               processor-node {:id (keyword (str (name (:id unused-category)) "-processor")) :kind :process :tag processor}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-category) :kind] category-kw)
                             (assoc-in [:attrs (:id exists-category-process) :kind] processed-kw)
                             (graph/add-node-with-attrs [(:id processor-node) processor-node])
                             (graph/add-directed-edge (:id unused-category) (:id processor-node))
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-process))
                             (graph/update-labels {:nodes [{:id (:id unused-category) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-process) :label-fn id+kind+tag->label}
                                                           {:id (:id processor-node) :label-fn id+kind+tag->label}]}))))}

               ;; unused-category + exists-category-process + exists-category-good =>
               ;;   category + category-good + category-processed + processor +
               ;;   [category processor] [processor category-good] [processor category-processed]
               {:match [[{:kind unused-kw}]
                        [{:kind exists-processed-kw}]
                        [{:kind exists-good-kw}]]
                :alter (fn [graph unused-categories exists-category-processeds exists-category-goods]
                         (let [unused-category (rand-nth unused-categories)
                               exists-category-processed (rand-nth exists-category-processeds)
                               exists-category-good (rand-nth exists-category-goods)
                               processor (rand-nth processes)
                               processor-node {:id (keyword (str (name (:id unused-category)) "-process")) :kind :process :tag processor}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-category) :kind] category-kw)
                             (assoc-in [:attrs (:id exists-category-processed) :kind] processed-kw)
                             (assoc-in [:attrs (:id exists-category-good) :kind] good-kw)
                             (graph/add-node-with-attrs [(:id processor-node) processor-node])
                             (graph/add-directed-edge (:id unused-category) (:id processor-node))
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-processed) {:label :=})
                             (graph/add-directed-edge (:id processor-node) (:id exists-category-good) {:label :=})
                             (graph/update-labels {:nodes [{:id (:id unused-category) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-processed) :label-fn id+kind+tag->label}
                                                           {:id (:id exists-category-good) :label-fn id+kind+tag->label}
                                                           {:id (:id processor-node) :label-fn id+kind+tag->label}]}))))}])))
        cat)
      categories)))

(defn step-economy [economy-data]
  #_(println
      (into []
        (map (fn [n] (select-keys n [:id :kind])))
        (graph/nodes (get-in land-data [::land/economy :ubergraph]))))
  (let [rules (generate-rules)]
    (update economy-data :ubergraph apply-rules rules)))

(comment
  (let [;; Extending the matching rules?
        ;;   https://www.metosin.fi/blog/malli-regex-schemas/
        rules [{:match [[{:kind :source}]
                        [{:kind :exists-fruit}]]
                :alter (fn [graph sources exists-fruits]
                         (let [source (rand-nth sources)
                               exists-fruit (rand-nth exists-fruits)]
                           (-> graph
                             (assoc-in [:attrs (:id exists-fruit) :kind] :unused-fruit)
                             (graph/add-directed-edge (:id source) (:id exists-fruit)))))}
               {:match [[{:kind :unused-fruit}]
                        [{:kind :exists-fruit-proc}]]
                :alter (fn [graph unused-fruits exists-fruit-procs]
                         (let [unused-fruit (rand-nth unused-fruits)
                               exists-fruit-proc (rand-nth exists-fruit-procs)
                               process (rand-nth processes)
                               process-node {:id (keyword (str (name (:id unused-fruit)) "-process")) :kind :process :tag process}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-fruit) :kind] :fruit)
                             (assoc-in [:attrs (:id exists-fruit-proc) :kind] :fruit-proc)
                             (graph/add-node-with-attrs [(:id process-node) process-node])
                             (graph/add-directed-edge (:id unused-fruit) (:id process-node))
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-proc)))))}
               {:match [[{:kind :unused-fruit}]
                        [{:kind :exists-fruit-proc}]
                        [{:kind :exists-fruit-good}]]
                :alter (fn [graph unused-fruits exists-fruit-procs exists-fruit-goods]
                         (let [unused-fruit (rand-nth unused-fruits)
                               exists-fruit-proc (rand-nth exists-fruit-procs)
                               exists-fruit-good (rand-nth exists-fruit-goods)
                               process (rand-nth processes)
                               process-node {:id (keyword (str (name (:id unused-fruit)) "-process")) :kind :process :tag process}]
                           (-> graph
                             (assoc-in [:attrs (:id unused-fruit) :kind] :fruit)
                             (assoc-in [:attrs (:id exists-fruit-proc) :kind] :fruit-proc)
                             (assoc-in [:attrs (:id exists-fruit-good) :kind] :fruit-good)
                             (graph/add-node-with-attrs [(:id process-node) process-node])
                             (graph/add-directed-edge (:id unused-fruit) (:id process-node))
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-proc) {:label :=})
                             (graph/add-directed-edge (:id process-node) (:id exists-fruit-good) {:label :=}))))}]
        ;; still need to figure out how to populate this with goods,
        ;;   processes can come from some randomness and process,
        ;;   maybe just do the same with goods for now?
        g {:nodes [{:id :source :kind :source}
                   {:id :sink :kind :sink}
                   {:id "Civ R+23" :kind :civ}
                   {:id :apple :kind :exists-fruit}
                   {:id :banana :kind :exists-fruit}
                   {:id :cream :kind :exists-fruit-good}
                   {:id :chop :kind :exists-fruit-proc}],
           :edges [[:source :sink]]}
        g (get-in @fruit-economy.core/*state [:world ::land/economy])
        g (graph/make g)]
    (nth (iterate #(apply-rules % rules) g) 3))

  ,)







;; WALKER
(comment
  ;; leaves -> root
  (let [g [[1 2] [3 4 [5 6]] [7 8]]]
    (walk/postwalk-demo g))

  ;; root -> leaves
  (let [g [[1 2] [3 4 [5 6]] [7 8]]]
    (walk/prewalk-demo g))

  (let [g {:nodes [{:id :source :color "green" :label "supply"}
                   {:id :sink :color "red" :label "demand"}]
           :edges [[:source :sink]]}]
    (-> g
      (graph/make)
      (->svg)))

  (let [kinds [:allowed-ore-proc :allowed-vehicle :allowed-plant-proc :allowed-food :allowed-housing :allowed-metal-part
               :allowed-plant-prod :allowed-ore :allowed-edible :allowed-plant :allowed-metal :allowed-machine :allowed-toy]
        rules [#{:source :allowed-plant}]
        g {:nodes [{:id :source :kind :source}
                   {:id :sink :kind :sink}
                   {:id :apple :kind :allowed-plant}
                   {:id :banana :kind :allowed-plant}],
           :edges [[:source :sink]]}]
    g
    (-> (graph/make g)
      (graph/nodes)
      (as-> $
        (reduce
          (fn [v node]
            (println rules)
            (conj v node))
          [] $))
      #_(graph/find-nodes {:kind :unused-fruit}))
    ;(walk/prewalk-demo g)
    ;(walk/prewalk-replace {:a 1 :b 2} g)
    ;(walk/postwalk-demo g)
    #_
    (doto fruit-economy.core/*state
      (swap! assoc-in [:world ::land/economy] g)
      (swap! update-in [:paused?] not))
    #_nil)

  (-> (get-in @fruit-economy.core/*state [:world ::land/economy])
    #_(->svg))
  ,)

(comment
  (let [g (uber/ubergraph true false
            [:Artemis {:population 3000}]
            [:Balela {:population 2000}]
            [:Coulton {:population 4000}]
            [:Dentana {:population 1000}]
            [:Egglesberg {:population 5000}]
            [:Artemis :Balela {:color :blue, :airline :CheapAir, :price 200, :distance 40}]
            [:Artemis :Balela {:color :green, :airline :ThriftyLines, :price 167, :distance 40}]
            [:Artemis :Coulton {:color :green, :airline :ThriftyLines, :price 235, :distance 120}]
            [:Artemis :Dentana {:color :blue, :airline :CheapAir, :price 130, :distance 160}]
            [:Balela :Coulton {:color :green, :airline :ThriftyLines, :price 142, :distance 70}]
            [:Balela :Egglesberg {:color :blue, :airline :CheapAir, :price 350, :distance 50}])]
    #_(uber/edges g)
    #_(uber/nodes g)
    (->svg g)
    #_(viz-graph g {:format :svg})))

(defn apply-graph [g {:keys [graph-fn node-fn edge-fn]}]
  (cond-> g
    graph-fn
    (graph-fn)

    node-fn
    (as-> $
      (reduce
        node-fn
        $
        (uber/nodes $)))

    edge-fn
    (as-> $
      (reduce
        edge-fn
        $
        (uber/edges $)))))

(defn consumables
  "computes amount of consumables at the sink"
  [g]
  (let [sink-edges (into [] (comp (map (juxt identity (partial uber/attrs g))) (filter (fn [[_ attrs]] (contains? (:tags attrs) :sink))) (map (comp (partial uber/in-edges g) first)) cat (map (fn [e] (merge (uber/attrs g e) e)))) (uber/nodes g))]
    (reduce
      (fn [v {:keys [src flow] :as edge}]
        (assoc v src flow))
      {}
      sink-edges)))

(defn linear-cost [consumption]
  (reduce-kv (fn [n _k v] (+ n v)) 0 consumption))

(defn sigmoid-1k
  "we do this because scaling by 0.004 means we're at 0.96 utility at 1000 units"
  [x]
  (dec (/ (* 2 (Math/exp (* 0.004 x))) (inc (Math/exp (* 0.004 x))))))

(defn sigmoid-cost [consumption]
  (reduce-kv (fn [n _k v] (+ n (sigmoid-1k v))) 0 consumption))

(defn raw-materials
  "find raw materials in the graph"
  [graph]
  (filterv (fn [n] (zero? (uber/in-degree graph n))) (uber/nodes graph)))

(defn sellable-products
  "find sellable products in the graph"
  [graph]
  (filterv (fn [n] (zero? (uber/out-degree graph n))) (uber/nodes graph)))

(defn init-weights
  [graph edge->weight relaxed?]
  (apply-graph graph
    {:graph-fn (fn [g]
                 (assoc g :negative-flows false))
     :node-fn (fn [g n]
                (uber/add-attrs g n {:quantity 0 :slack 0}))
     :edge-fn (fn [g e]
                (let [init-weight (get (uber/attrs g e) :init-weight)
                      weight (or init-weight (get edge->weight e (if relaxed? 1 Long/MAX_VALUE)))]
                  (uber/add-attrs g e {:flow (or init-weight 0) :min-flow (or init-weight 0) :weight weight})))}))

(comment
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

        g (uber/ubergraph true false
            :apple
            :juice
            :butter
            [:sink {:tags #{:sink}}]
            [:apple :butter {#_#_:color :blue :label "3X" :loss 1/2}]
            [:apple :juice {#_#_:color :blue :label "2X"}]
            [:apple :sink {#_#_:color :blue :label "1X"}]
            [:butter :sink]
            [:juice :sink])
        ;;#_#_
        g (uber/ubergraph true false
            [:CHERRY-BLOSSOM {:track 30 :tags #{:plant}}]
            [:ROSE {:track 36 :tags #{:plant}}]
            [:LEMON {:track 29 :tags #{:plant}}]
            [:BASKET {:track 32 :tags #{:process}}]
            [:GHOST {:track 34 :tags #{:factory}}]
            [:MAGE {:track 42 :tags #{:process}}]
            [:CANNED-FOOD {:track 31 :tags #{:food}}]
            [:CUP {:track 35 :tags #{:plant-proc}}]
            [:HAMBURGER {:track 33 :tags #{:food}}]
            [:KITCHEN-KNIFE {:track 38 :tags #{:process}}]
            [:ATOM {:track 40 :tags #{:process}}]
            [:DUMPLING {:track 37 :tags #{:food}}]
            [:SPARKLE {:track 39 :tags #{:plant-prod}}]
            [:MICROBE {:track 41 :tags #{:plant-proc}}]
            [:BEVERAGE-BOX {:track 43 :tags #{:plant-proc}}]
            [:source {:tags #{:source}}]
            [:source :ROSE {:init-weight 1000}]
            [:source :CHERRY-BLOSSOM {:init-weight 1000}]
            [:sink {:tags #{:sink}}]
            [:LEMON :sink]
            [:LEMON :BASKET]
            [:LEMON :GHOST]
            [:LEMON :MAGE]
            [:CHERRY-BLOSSOM :sink]
            [:CANNED-FOOD :sink]
            [:BASKET :CANNED-FOOD]
            [:BASKET :CUP]
            [:HAMBURGER :sink]
            [:GHOST :HAMBURGER]
            [:CUP :GHOST]
            [:ROSE :KITCHEN-KNIFE]
            [:ROSE :ATOM]
            [:DUMPLING :sink]
            [:KITCHEN-KNIFE :DUMPLING]
            [:KITCHEN-KNIFE :SPARKLE]
            [:ATOM :CUP]
            [:ATOM :MICROBE]
            [:MAGE :CUP]
            [:MAGE :BEVERAGE-BOX])
        nodes (uber/nodes g)
        ;; proportional weights?
        simplices (into []
                    (comp
                      (filter (fn [n] (let [tags (:tags (uber/attrs g n))]
                                        (and
                                          ;; don't adjust weights on factories / processes. These are more static and only change with techs / staff / skill changes. Same with source which should change differently.
                                          (not (some #{:factory :process :source} tags))
                                          (> (uber/out-degree g n) 1)))))
                      (map (fn [n] (into [] (uber/out-edges g n)))))
                    nodes)

        ;; Here we're just doing a start weighted to the size of the simplex, so each option is evenly weighted.
        ;;   We could also set our own weights, with the assumption that they add to one.
        relaxed-start-weights (mapv (fn [simplex] (let [size (count simplex)] (vec (repeat size (/ 1 size))))) simplices)
        relaxed? true

        edge-weights (mapv zipmap simplices relaxed-start-weights)
        edge->weight (into {} edge-weights)

        ;; compute flow through the network
        computed-flow
        #_
        (fn [graph]
          (reduce
            (fn [g n]
              (let [in-edges (uber/in-edges g n)
                    in-degree (count in-edges)
                    out-edges (uber/out-edges g n)
                    out-degree (count out-edges)
                    in-edge+attrs (into {} (map (juxt identity (partial uber/attrs g))) in-edges)
                    out-edge+attrs (into {} (map (juxt identity (partial uber/attrs g))) out-edges)

                    ;; TODO: compare reduce-kv vs transduce for same sized input
                    [sum-flow min-flow] (reduce-kv (fn [[sum mn] e {:keys [flow] :as attrs}] [(+ flow sum) (min mn flow)]) [0 Long/MAX_VALUE] in-edge+attrs)
                    min-flow-required (transduce (map (comp :min-flow second)) + 0 out-edge+attrs)

                    node-attrs (uber/attrs g n)
                    tags (:tags node-attrs)
                    source? (contains? tags :source)

                    quantity (+ sum-flow (:quantity node-attrs))
                    available (- quantity min-flow-required)]
                ;; update :flow based on
                ;;   loss
                ;;   if it takes multiple inputs then:
                ;;     take the minimum of their amounts
                ;;     and then multiply it by the number of inputs for the output
                ;;   if it gives multiple outputs then:
                ;;     split the output equally
                (println :inputs in-degree :outputs out-degree)
                (println :source? source? :available available :quantity quantity :sum-flow sum-flow :min-flow min-flow :min-flow-required min-flow-required)

                (cond
                  (not source?)
                  (as-> g $
                    (uber/add-attrs $ n {:quantity quantity})
                    (reduce
                      (fn [g' [e {:keys [weight min-flow flow loss] :as attrs}]]
                        (println :loss loss :min-flow min-flow :new-flow (* weight available) :attrs attrs out-degree)
                        (if (zero? min-flow)
                          (uber/add-attrs g' e {:flow (* weight available)})
                          g'))
                      $
                      out-edge+attrs))

                  :else
                  g)

                #_(println
                    (cond-> {}
                      (and (not source?) (> in-degree 1))
                      (->
                        (assoc :node-fn (fn node-fn+in-degree [g n]
                                          (println :node-fn :in-degree n)
                                          (uber/add-attrs g n {:quantity quantity})))
                        (assoc :edge-fn (fn edge-fn+in-degree [g [e {:keys [weight min-flow flow loss] :as attrs}]]
                                          (println :edge-fn :in-degree e)
                                          (uber/add-attrs g e {:flow (* weight available)}))))

                      (and (not source?) (> out-degree 1))
                      (->
                        (assoc :node-fn (fn node-fn+out-degree [g n]
                                          (println :node-fn :out-degree n)
                                          (println :node-attrs node-attrs)
                                          (println :node-attrs-fresh (uber/attrs g n))
                                          (uber/add-attrs g n {:quantity quantity})))
                        (assoc :edge-fn (fn edge-fn+out-degree [g [e {:keys [weight min-flow flow loss] :as attrs}]]
                                          (println :edge-fn :out-degree e)
                                          (println :node-attrs attrs)
                                          (println :node-attrs-fresh (uber/attrs g e))
                                          (uber/add-attrs g e {:flow (* weight available)}))))

                      (not source?)
                      (->
                        (assoc :node-fn (fn node-fn+out-degree [g n]
                                          (println :node-fn :out-degree n)
                                          (println :node-attrs node-attrs)
                                          (println :node-attrs-fresh (uber/attrs g n))
                                          (uber/add-attrs g n {:quantity quantity})))
                        (assoc :edge-fn (fn edge-fn+out-degree [g [e {:keys [weight min-flow flow loss] :as attrs}]]
                                          (println :edge-fn :out-degree e)
                                          (println :node-attrs attrs)
                                          (println :node-attrs-fresh (uber/attrs g e))
                                          (uber/add-attrs g e {:flow (* weight available)})))))
                    in-edge+attrs)))
            graph
            (uber.alg/topsort graph)))
        (fn [graph]
           (reduce
             (fn [g n]
               (let [in-edges (uber/in-edges g n)
                     in-degree (count in-edges)
                     out-edges (uber/out-edges g n)
                     out-degree (count out-edges)
                     in-edge+attrs (into {} (map (juxt identity (partial uber/attrs g))) in-edges)
                     out-edge+attrs (into {} (map (juxt identity (partial uber/attrs g))) out-edges)

                     ;; TODO: compare reduce-kv vs transduce for same sized input
                     [sum-flow min-flow] (reduce-kv (fn [[sum mn] e {:keys [flow] :as attrs}] [(+ flow sum) (min mn flow)]) [0 Long/MAX_VALUE] in-edge+attrs)
                     min-flow-required (transduce (map (comp :min-flow second)) + 0 out-edge+attrs)

                     node-attrs (uber/attrs g n)
                     tags (:tags node-attrs)
                     source? (contains? tags :source)]
                 ;; update :flow based on
                 ;;   loss
                 ;;   if it takes multiple inputs then:
                 ;;     take the minimum of their amounts
                 ;;     and then multiply it by the number of inputs for the output
                 ;;   if it gives multiple outputs then:
                 ;;     split the output equally
                 (println)
                 (println n (:track node-attrs))
                 (println :inputs in-degree :outputs out-degree :in-edge+attrs in-edge+attrs)

                 (cond
                   (and (not source?) (contains? tags :process))
                   (let [quantity (+ sum-flow (:quantity node-attrs))]
                     (println :source? source? :quantity quantity :sum-flow sum-flow :min-flow min-flow :min-flow-required min-flow-required)
                     (as-> g $
                       (uber/add-attrs $ n {:quantity quantity})
                       (reduce
                         (fn [g' [e {:keys [weight flow loss] :as attrs}]]
                           (let [div-fn (if relaxed? / quot)
                                 ;; split evenly between outputs
                                 output-split (div-fn quantity out-degree)]
                             (println :PROCESS :loss loss :output-split output-split :new-flow output-split :edge e :attrs attrs out-degree)
                             (uber/add-attrs g' e {:flow output-split})))
                         $
                         out-edge+attrs)))

                   (and (not source?) (contains? tags :factory))
                   (let [produced (* min-flow in-degree)
                         quantity (+ produced (:quantity node-attrs))]
                     (println :source? source? :quantity quantity :sum-flow sum-flow :min-flow min-flow :min-flow-required min-flow-required)
                     (as-> g $
                       (uber/add-attrs $ n {:quantity quantity})
                       (reduce
                         (fn [g' [e {:keys [weight flow loss] :as attrs}]]
                           (println :PROCESS :loss loss :quantity quantity :new-flow quantity :edge e :attrs attrs out-degree)
                           (uber/add-attrs g' e {:flow quantity}))
                         $
                         out-edge+attrs)))

                   (not source?)
                   (let [quantity (+ sum-flow (:quantity node-attrs))
                         available (- quantity min-flow-required)]
                     (println :source? source? :available available :quantity quantity :sum-flow sum-flow :min-flow min-flow :min-flow-required min-flow-required)
                     (as-> g $
                       (uber/add-attrs $ n {:quantity quantity})
                       (reduce
                         (fn [g' [e {:keys [weight min-flow flow loss] :as attrs}]]
                           (println :BRANCH :loss loss :min-flow min-flow :new-flow (* weight available) :edge e :attrs attrs out-degree)
                           (if (zero? min-flow)
                             (uber/add-attrs g' e {:flow (* weight available)})
                             g'))
                         $
                         out-edge+attrs)))

                   :else
                   g)))
             graph
             (uber.alg/topsort graph)))

        gradient-state-fn (fn [g {:keys [edge->weight] :as state}]
                            (let [flow-graph (computed-flow g)
                                  consumption (consumables flow-graph)
                                  cost (sigmoid-cost consumption)]
                              (merge
                                (update state :history (fnil conj []) {:weights edge->weight :cost cost})
                                {:flow-graph flow-graph
                                 :consumption consumption
                                 :edge->weight edge->weight
                                 :cost cost
                                 :beta 0.4
                                 :step 0.1
                                 :num-iterations 0
                                 :num-successes 0})))

        init-state (fn [{:keys [flow-graph edge->weight simplices relaxed?] :as state}]
                     (let [init-graph (init-weights flow-graph edge->weight relaxed?)]
                       (gradient-state-fn init-graph state)))

        calc-gradients (fn [{:keys [flow-graph edge->weight cost] :as st}]
                         (reduce-kv
                           (fn [m edge weight]
                             (let [delta 1/1000
                                   edge->weight' (assoc edge->weight edge (+ weight delta))
                                   graph (init-weights flow-graph edge->weight' relaxed?)
                                   flow-graph' (computed-flow graph)
                                   consumption' (consumables flow-graph')
                                   cost' (sigmoid-cost consumption')]
                               (println :flow-graph' :cost cost' :consumption consumption' :edge edge)
                               (uber/pprint flow-graph')
                               (assoc m edge (/ (- cost' cost) delta))))
                           {}
                           edge->weight))
        step-weights (fn [{:keys [edge->weight step] :as state}]
                       (let [gradients (calc-gradients state)]
                         (-> state
                           (assoc :prior-edge->weight edge->weight)
                           (update :edge->weight (fn [edge->weight]
                                                   (merge-with
                                                     (fn [weight gradient]
                                                       (+ weight (* step gradient)))
                                                     edge->weight gradients))))))
        project-weights-onto-simplex (fn [{:keys [edge->weight simplices] :as state}]
                                       (let [project-onto-simplex (fn [xs]
                                                                    (let [size (count xs)
                                                                          sorted-xs (vec (sort > xs))
                                                                          [sum t] (reduce
                                                                                    (fn [[sum t] [idx x next-x]]
                                                                                      (let [sum' (+ sum x)
                                                                                            t' (/ (- sum' 1) idx)]
                                                                                        (if (>= t next-x)
                                                                                          (reduced [sum' t'])
                                                                                          [sum' t'])))
                                                                                    [0 0]
                                                                                    (map vector (range 1 (inc size)) sorted-xs (conj (subvec sorted-xs 1) 0)))]
                                                                      (mapv (fn [x] (max 0 (- x t))) xs)))]
                                         (reduce
                                           (fn [state simplex]
                                             (let [simplex-weights (mapv edge->weight simplex)]
                                               (reduce-kv
                                                 (fn [state' edge adjusted-weight]
                                                   (assoc-in state' [:edge->weight edge] adjusted-weight))
                                                 state
                                                 (zipmap simplex (project-onto-simplex simplex-weights)))))
                                           state
                                           simplices)))

        update-consumption+cost (fn [{:keys [flow-graph edge->weight consumption cost] :as state}]
                                  (let [flow-graph' (computed-flow flow-graph)
                                        consumption' (consumables flow-graph')
                                        cost' (sigmoid-cost consumption')]
                                    (assoc state :cost cost' :consumption consumption'
                                      :prior-cost cost :prior-consumption consumption)))

        state->state (fn [state]
                       (-> (init-state state)
                         (step-weights)
                         (project-weights-onto-simplex)
                         (update-consumption+cost)))]


    (let [state {:simplices simplices :relaxed? relaxed? :edge->weight edge->weight :flow-graph g}

          ;; so basically init-weights?
          init-goods 1000
          fixed-weights {:ROSE init-goods :LEMON init-goods}]

      ;; assume that fixed resources, source + sink are in graph with edges correctly attached.

      ;; post setting weightings
      #_
      (-> (init-weights g edge->weight relaxed?)
        (gradient-state-fn state)
        (step-weights)
        (project-weights-onto-simplex)
        (init-state state)

        #_(computed-flow)
        #_(gradient-state-fn state)
        #_(uber/pprint))

      ;; 1. Initial Flow Calc
      ;; 2. Setup State

      ;; 3. IF recalc-gradients, do that
      ;; 4. Use gradients to create new weights using step + old weights
      ;; 5. Project new weights onto simplex
      ;; 6. Calculate new flow
      ;; 7. Compare new and old weights to work out direction
      ;; 8. Compare new and old directions to work out angle
      ;; 9. Compute old / new cost and if improvement adjust step size
      ;; 10. Compute old / new cost and adjust step size

      #_ ;; just run and check out the cost function
      (:history (nth (iterate state->state state) 50))

      #_
      (-> (state->state state)
        (state->state)
        (state->state)
        ((fn [{:keys [edge->weight prior-edge->weight step beta cost prior-cost] :as state}]
           (println :x :prior-edge->weight :x_p :edge->weight)

           (println :edge->weight (update-keys edge->weight (juxt :src :dest)) :prior-edge->weight (update-keys prior-edge->weight (juxt :src :dest)))
           (let [dot-product (fn [m1 m2] (reduce-kv (fn [n k v] (+ n v)) 0 (merge-with * m1 m2)))
                 pow-sum (fn [m] (reduce-kv (fn [n k v] (+ n (* v v))) 0 m))

                 direction-fn (fn [weights-old weights-new] (merge-with - weights-new weights-old))
                 directional-angle-fn (fn [weights-old weights-new]
                                        (let [dot-product (dot-product weights-old weights-new)
                                              magnitude (Math/sqrt (* (pow-sum weights-old) (pow-sum weights-new)))]
                                          (if (zero? magnitude)
                                            Math/PI
                                            (let [cos-theta (max (min 1 (/ dot-product magnitude)) -1)]
                                              (Math/acos cos-theta)))))
                 effective-grad-fn (fn [weights-old weights-new]
                                     (reduce-kv
                                       (fn [m k v] (assoc m k (/ v step)))
                                       {}
                                       (merge-with - weights-new weights-old)))
                 grad-squared-fn (fn [weights-m] (pow-sum weights-m))
                 bound-fn (fn [weights-old weights-new step] (* (/ step 2) (grad-squared-fn (effective-grad-fn weights-old weights-new))))
                 {:keys [directional-angle bound]} {:direction (direction-fn prior-edge->weight edge->weight)
                                                    :directional-angle (directional-angle-fn prior-edge->weight edge->weight)
                                                    :effective-grad (effective-grad-fn prior-edge->weight edge->weight)
                                                    :bound (bound-fn prior-edge->weight edge->weight step)
                                                    :cost cost
                                                    :prior-cost prior-cost}]
             (cond
               (<= (- cost prior-cost) bound)
               (let [step' (* step beta)]
                 (println :stop? (< step' 0.000001))
                 (if (< step' 0.000001)
                   (assoc state :step step')))

               ;;elif curr_angle is not None and curr_angle < 0.005 and y_p > y:
               ,))))
        #_#_
        :history
        ((fn [history]
           (let [direction-fn (fn [weights-u weights-v] (merge-with - weights-v weights-u))
                 [vectors _] (reduce
                               (fn [[vectors last-weights] {:keys [weights] :as entry}]
                                 [(conj vectors (direction-fn last-weights weights)) weights])
                               [[] (:weights (first history))]
                               (rest history))

                 directional-angle-fn (fn [weights-u weights-v]
                                        (let [dot-product (merge-with * weights-u weights-v)
                                              pow-sum (fn [m] (reduce-kv (fn [n k v] (+ n (* v v))) 0 m))
                                              magnitude (Math/sqrt (* (pow-sum weights-u) (pow-sum weights-v)))]
                                          (if (zero? magnitude)
                                            Math/PI
                                            (let [cos-theta (max (apply min 1 (map #(/ % magnitude) (vals dot-product))) -1)]
                                              (Math/acos cos-theta)))))
                 [angles _] (reduce
                              (fn [[vectors last-weights] weights]
                                [(conj vectors (directional-angle-fn last-weights weights)) weights])
                              [[] (first vectors)]
                              (rest vectors))]
             angles)
           #_(apply merge-with - (mapv (fn [entry] (:weights entry)) history))))
        ;; Compute angle between previous step and this one.
        #_(uber/pprint))

      #_
      ;; set raw materials to init-goods
      ;; Split this up into fixing weights + marking raw materials
      (as-> g $
        ((fn fix-raw-material-weights [graph fixed-weights]
           (reduce
             (fn [g material]
               (if-let [init-weight (get fixed-weights material)]
                 (uber/build-graph g
                   [:source {:tags #{:source}}]
                   [:source material {:init-weight init-weight}])
                 g))
             graph
             (raw-materials graph)))
         $ fixed-weights)

        ((fn add-products [graph products]
           (reduce
             (fn [g material]
               (if-let [init-weight (get fixed-weights material)]
                 (uber/build-graph g
                   [:source material {:init-weight init-weight}])
                 g))
             graph
             products))
         $ [:DUMPLING :BEVERAGE-BOX :CANNED-FOOD :HAMBURGER :SPARKLE :MICROBE :CHERRY-BLOSSOM]))

      #_
      (into []
        (comp
          (filter (fn [n] (and (not (some #{:factory :process} (:tags (uber/attrs g n)))) (> (uber/out-degree g n) 1))))
          (map (fn [n] (into [] (uber/out-edges g n)))))
        nodes)
      #_(->
          ;; set raw materials to init-goods
          (reduce
            (fn [g material]
              (uber/build-graph g
                [:source {:tags #{:source}}]
                [:source material {:init-weight init-goods}]))
            g
            raw-materials)
          ;; post setting weightings
          (init-weights edge->weight relaxed?)
          (gradient-state-fn state)
          (step-weights)
          (project-weights-onto-simplex)
          :flow-graph
          (uber/pprint)))

    #_
    (-> g''
      (gradient-state-fn)
      :weights
      #_(uber/pprint))
    #_
    (apply-graph g''
      {:edge-fn (fn [g e]
                  (println (uber/in-edges) :attr (uber/attrs g e) :e e)
                  g)})
    #_(as-> g' $
        (assoc $ :negative-flows false)
        (reduce
          (fn [g n]
            (uber/add-attrs g n {:quantity 0 :slack 0}))
          $
          (uber/nodes $))
        (reduce
          (fn [g e]
            (println e :simplices simplices)
            (uber/add-attrs g e {:flow 0 :min-flow 0 :weight (if relaxed? 1 Long/MAX_VALUE)}))
          $
          (uber/edges $)))
    ;; Now to compute flow through the network
    #_(uber.alg/topsort g')))

(comment
  (let [planning 10
        inventory {:apple 10 :banana 12}
        buying {:apple 10 :banana 12}
        current (update-vals inventory sigmoid-1k)]
    (merge-with - (update-vals (merge-with + inventory buying) sigmoid-1k) current)
    #_(merge-with - (update-vals inventory (comp sigmoid-1k (partial + 10))) current))

  (let [inits (into []
                cat
                (for [idx (range 1)]
                  [[idx {:label :farmer :tags #{:farmer :grain} :produces 10 :inventory 100}]
                   [(inc idx) {:label :miller :tags #{:process}}]
                   [:ROSE {:tags #{:plant}}]
                   [:LEMON {:tags #{:plant}}]
                   [:BASKET {:tags #{:process}}]
                   [:GHOST {:tags #{:factory}}]
                   [:ROSE :BASKET]
                   [:LEMON :BASKET]
                   [:BASKET :GHOST]]))
        graph (apply uber/ubergraph true false
                inits)

        simplices (fn [graph]
                    (into []
                      (comp
                        (filter (fn [n] (let [tags (:tags (uber/attrs graph n))]
                                          (and
                                            ;; don't adjust weights on factories / processes. These are more static and only change with techs / staff / skill changes. Same with source which should change differently.
                                            (not (some #{:factory :process :source} tags))
                                            (> (uber/out-degree graph n) 1)))))
                        (map (fn [n] (into [] (uber/out-edges graph n)))))
                      (uber/nodes graph)))]
    #_(uber/pprint graph)
    (simplices graph))

  ,)
