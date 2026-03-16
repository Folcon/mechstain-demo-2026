(ns fruit-economy.graph.viz
  (:require [fruit-economy.graph.ubergraph :as graph]
            [hiccup.core :refer [html]])
  (:import [guru.nidi.graphviz.model Factory Link MutableNode MutableGraph]
           [guru.nidi.graphviz.engine Format Graphviz Engine]
           [guru.nidi.graphviz.attribute Color Label]))


(defn ubergraph->svg [ubergraph]
  (let [g (-> (Factory/mutGraph)
            (.setDirected true))
        data {:graph g :nodes {} :edges {}}]
    (as-> data $
      (reduce
        (fn [g {:keys [id color label] :as node}]
          (let [node-id (name id)
                new-node (cond-> (Factory/mutNode ^String node-id)
                           color
                           (.add (Color/named (name color)))
                           label
                           (.add (Label/html (html label))))]
            (-> g
              (update :nodes assoc node-id new-node)
              (update :graph #(.add % [new-node])))))
        $
        (graph/nodes ubergraph))

      (reduce
        (fn [{:keys [nodes] :as g} {:keys [id src dest] :as edge}]
          (let [edge-id (str id)
                src-id (name src)
                dest-id (name dest)
                src-node (get nodes src-id)
                dest-node (get nodes dest-id)
                _ (.addLink ^MutableNode src-node [(Link/to dest-node)])]
            g))
        $
        (graph/edges ubergraph))

      (-> (Graphviz/fromGraph ^MutableGraph (:graph $))
        (.engine Engine/FDP)
        (.width 1200)
        (.height 800)
        (.render Format/SVG)
        (.toString)))))

(comment
  (html)

  (let [g (Factory/mutGraph)
        n (.add (Factory/mutNode "test") (Label/markdown "<table border=\"0\"><tr><td border=\"1\">clojure.core$name@239da94d</td></tr><tr><td border=\"1\">clojure.core$name@239da94d</td></tr><tr><td border=\"1\">clojure.core$name@239da94d</td></tr></table>"))]
    (.add g n)
    (-> (Graphviz/fromGraph ^MutableGraph g)
      (.engine Engine/CIRCO)
      (.width 1200)
      (.height 800)
      (.render Format/SVG)
      (.toString))))

(comment
  (let [ugraph (graph/make
                 {:nodes [{:id :Artemis :population 3000}
                          {:id :Balela :population 2000}
                          {:id :Coulton :population 4000}
                          {:id :Dentana :population 1000}
                          {:id :Egglesberg :population 5000}]
                  :edges [[:Artemis :Balela {:color :blue :airline :CheapAir :price 200 :distance 40}]
                          [:Artemis :Balela {:color :green :airline :ThriftyLines :price 167 :distance 40}]
                          [:Artemis :Coulton {:color :green :airline :ThriftyLines :price 235 :distance 120}]
                          [:Artemis :Dentana {:color :blue :airline :CheapAir :price 130 :distance 160}]
                          [:Balela :Coulton {:color :green :airline :ThriftyLines :price 142 :distance 70}]
                          [:Balela :Egglesberg {:color :blue :airline :CheapAir :price 350 :distance 50}]]})]
    #_(uber/edges g)
    #_(uber/nodes g)

    (let [g (-> (Factory/mutGraph)
              (.setDirected true))
          data {:graph g :nodes {} :edges {}}]
      (as-> data $
        (reduce
          (fn [g {:keys [id color] :as node}]
            (let [node-id (name id)
                  new-node (Factory/mutNode ^String node-id)]
              (-> g
                (update :nodes assoc node-id new-node)
                (update :graph #(.add % [new-node])))))
          $
          (graph/nodes ugraph))

        (reduce
          (fn [{:keys [nodes] :as g} {:keys [id src dest] :as edge}]
            (let [edge-id (str id)
                  src-id (name src)
                  dest-id (name dest)
                  src-node (get nodes src-id)
                  dest-node (get nodes dest-id)
                  _ (.addLink ^MutableNode (get nodes src-id) [(Link/to dest-node)])]
              ; (clojure.pprint/pprint (clojure.reflect/reflect))
              ;(println ) #_(Link/between (.asLinkSource (get nodes src-id)) (.asLinkTarget dest-node))
              (-> g
                ;(update :edges assoc node-id new-node)
                #_(update-in [:nodes src-id] #(.addLink % dest-node)))))
          $
          (graph/edges ugraph))

        (-> (Graphviz/fromGraph ^MutableGraph (:graph $))
          (.width 600)
          (.render Format/SVG)
          (.toString))

        #_(let [g (-> (Factory/graph)
                    (.directed))
                data {:graph g :nodes {} :edges {}}]
            (as-> data $
              (reduce
                (fn [g {:keys [id color] :as node}]
                  (let [node-id (name id)
                        new-node (Factory/node ^String node-id)]
                    (-> g
                      (update :nodes assoc node-id new-node)
                      (update :graph #(.with % [new-node])))))
                $
                (graph/nodes ugraph))
              (reduce
                (fn [{:keys [nodes] :as g} [src dest attrs]]
                  (let [src-id (name src)
                        dest-id (name dest)
                        src-node (get nodes src-id)
                        dest-node (get nodes dest-id)]
                    (println (.asLinkTarget src-node) #_(.to (.link (:graph g) (.asLinkTarget src-node)) dest-node))
                    g
                    #_(-> g
                        (update :nodes assoc node-id new-node)
                        (update :graph #(.with % [new-node])))))
                $
                (graph/edges ugraph))))))))