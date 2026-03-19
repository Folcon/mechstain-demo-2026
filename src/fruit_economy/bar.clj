(ns fruit-economy.bar
  (:require [ubergraph.core :as uber]))


(let [factory-graph (uber/multidigraph
                      [:coal :factory {:quantity 10}]
                      [:iron :factory {:quantity 20}]
                      [:factory :steel {:quantity 8}])
      cities-graph (uber/multidigraph
                     [:london :cambridge {:distance 10}]
                     [:cambridge :london {:distance 10}]
                     [:london :oxford {:quantity 20}]
                     [:oxford :london {:quantity 20}]
                     [:cambridge :oxford {:quantity 8}]
                     [:oxford :cambridge {:quantity 8}])]
  (-> cities-graph
    (uber/build-graph factory-graph)
    (uber/pprint)))
