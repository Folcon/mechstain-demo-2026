(ns inkstain.persistence
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import [java.io File]))



(def default-comp
  {:player {:chassis :medium :drive-train :standard}
   :allies [{:chassis :light  :drive-train :dasher   :enabled? true}
            {:chassis :light  :drive-train :standard :enabled? true}
            {:chassis :heavy  :drive-train :standard :enabled? true}
            {:chassis :medium :drive-train :charger  :enabled? true}
            {:chassis :medium :drive-train :standard :enabled? true}]})

(defn save-file []
  (io/file (System/getProperty "user.home") ".inkstain" "mechstain.edn"))

(defn load-data []
  (let [^File f (save-file)]
    (if (.exists f)
      (edn/read-string (slurp f))
      {:last-used "default"
       :presets {"default" default-comp}})))

(defn save-data! [data]
  (let [^File f (save-file)]
    (.mkdirs (.getParentFile f))
    (spit f (pr-str data))))

(defn save-preset! [preset-name comp]
  (let [data (load-data)]
    (save-data! (-> data
                  (assoc-in [:presets preset-name] comp)
                  (assoc :last-used preset-name)))))

(def max-custom-presets 3)

(defn next-preset-name [presets]
  (let [existing (disj (set (keys presets)) "default")]
    (when (< (count existing) max-custom-presets)
      (first (remove existing
               (map #(str "custom-" %) (range 1 (inc max-custom-presets))))))))
