(ns build
  (:require [clojure.string :as str]
            [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def uber-file "target/uber.jar")

(defn- get-version
  "Get version from RELEASE_VERSION env var (CI), git tag, or fallback."
  []
  (or (System/getenv "RELEASE_VERSION")
      (let [tag (try
                  (-> (Runtime/getRuntime)
                      (.exec (into-array ["git" "describe" "--tags" "--exact-match"]))
                      (.getInputStream)
                      (slurp)
                      (str/trim))
                  (catch Exception _ nil))]
        (when (and tag (str/starts-with? tag "v"))
          (subs tag 1)))
      (let [sha (try
                  (-> (Runtime/getRuntime)
                      (.exec (into-array ["git" "rev-parse" "--short" "HEAD"]))
                      (.getInputStream)
                      (slurp)
                      (str/trim))
                  (catch Exception _ "unknown"))]
        (str "dev-" sha))))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-java
  "Compile Java sources to classes directory."
  [_]
  (println "Compiling Java sources...")
  (b/javac {:src-dirs ["src/java"]
            :class-dir class-dir
            :basis (b/create-basis {:project "deps.edn"})}))

(defn uber [_]
  (clean nil)
  (println (str "Building uberjar"))

  ;; Compile Java first
  (compile-java nil)

  ;; Copy Clojure sources
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})

  ;; Write version file
  (let [version (get-version)]
    (println (str "Version: " version))
    (spit (str class-dir "/version.txt") version))

  ;; Create basis with platform-specific dependencies
  (let [basis (b/create-basis {:project "deps.edn"})]

    ;; Build uberjar
    (b/uber {:class-dir class-dir
             :uber-file uber-file
             :basis basis
             :main 'inkstain.core}))

  (println (str "Built: " uber-file)))
