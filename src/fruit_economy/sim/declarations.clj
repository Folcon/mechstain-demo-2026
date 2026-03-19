(ns fruit-economy.sim.declarations)

(comment
  Thanks to @seancorfield for this approach!
  dev=> (defprotocol Basic :extend-via-metadata true (do-it [_]))
  Basic
  dev=> (extend-protocol Basic Object (do-it [_] (println "Object!")))
  nil
  dev=> (do-it {:a 1})
  Object!
  nil
  dev=> (do-it (with-meta {:a 1} {`do-it (fn [_] (println "Special!"))}))
  Special!
  nil)

;; Multi
(defmulti tick
  (fn [entity land-data] (:kind entity)))

(defmethod tick :default [entity land-data]
  (println "UNHANDLED ENTITY: " (:kind entity (pr-str entity))))

(defmulti decide
  (fn [entity land-data] (:kind entity)))

;(defmulti IEntity
;  (tick [this land-data]
;    "Do stuff"))
;
;(defprotocol ILeader
;  (decide [this land-data]
;    "Picks a choice between various actions"))
