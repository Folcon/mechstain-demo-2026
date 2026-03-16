(ns inkstain.version
  "Build version utilities"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))



(def ^:private version-cache (atom nil))

(defn get-version
  "Get the build version. Reads from version.txt in resources (created at build time),
   or falls back to dev version based on git"
  []
  (or @version-cache
      (let [v (or
                ;; Try to read from bundled version.txt (release builds)
               (when-let [resource (io/resource "version.txt")]
                 (str/trim (slurp resource)))
                ;; Fallback for dev: try git
               (try
                 (let [sha (-> (Runtime/getRuntime)
                               (.exec (into-array ["git" "rev-parse" "--short" "HEAD"]))
                               (.getInputStream)
                               (slurp)
                               (str/trim))]
                   (str "dev-" sha))
                 (catch Exception _ nil))
                ;; Ultimate fallback
               "dev")]
        (reset! version-cache v)
        v)))

(defn version-string
  "Get formatted version string for display"
  [engine-name]
  (str engine-name " " (get-version)))
