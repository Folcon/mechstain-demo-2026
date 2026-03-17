(ns inkstain.utils
  (:import [clojure.lang PersistentQueue]))



(defmethod print-method PersistentQueue [q ^java.io.Writer w]
  (.write w "#queue ")
  (print-method (sequence q) w))

(defn queue [coll]
  (into PersistentQueue/EMPTY coll))

(defn clamp [v lo hi]
  (max lo (min hi v)))
