(ns fruit-economy.sim.core-test
  (:require [clojure.test :refer :all]
            [datascript.core :as d]
            [fruit-economy.land :as land]
            [fruit-economy.economy :as economy]
            [fruit-economy.civ :as civ]
            [fruit-economy.core :refer [init-world]]
            [fruit-economy.db.core :as db]
            [fruit-economy.data.core :as data]
            [fruit-economy.core :as core])
  (:import [java.lang AutoCloseable]))


(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest process-decision-test
  (let []))

(comment
  (let [width  10
        height 10
        world (init-world "World" width height)]
    (db/db-bulk-insert (db/init-db) [world])))

;; Let's start with a situation when no action is planned
(comment
  (let [width  3
        height 3
        land (-> (land/make-land "World" width height)
               (land/gen-land))

        {::land/keys [name terrain curr-civ-id civ-letters lang] :as land-data} land-data
        symbol (first civ-letters)
        civ-name (lang/make-word lang)
        biome (get-in terrain [y x])
        first-name (lang/make-word lang)
        last-name (lang/make-word lang)
        new-peep {::civ/id curr-civ-id
                  ::civ/name civ-name
                  :name (str first-name " " last-name)
                  :first-name first-name
                  :last-name last-name
                  :kind :peep
                  :glyph "🧑"
                  :decisions [:claim :develop :gather :grow]}
        new-civ (civ/make-civ curr-civ-id civ-name symbol [x y] name biome parent [new-peep])]
    land))

(comment
  (def l
    (let [width  60
          height 40
          land (-> (land/make-land "World" width height)
                 (land/gen-land)
                 (land/populate 50 #_100)
                 (land/spawn-units 10)
                 (economy/add-resources)
                 (civ/try-spawn-new-civs 10))]
      land))
  (let [land l]
    (keys land)))

;; Sorting out datascript query perf

(def world
  (let [width  20
        height 25]
    (core/init-world "World" width height)))

(def world-db-unindexed
  (let [db (d/empty-db {:land/history {:db/valueType :db.type/ref
                                       :db/cardinality :db.cardinality/many}
                        :land/resources {:db/valueType :db.type/ref
                                         :db/cardinality :db.cardinality/many}
                        :land/units {:db/valueType :db.type/ref
                                     :db/cardinality :db.cardinality/many}
                        :land/civs {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}
                        :civ/territory {:db/valueType :db.type/ref
                                        :db/cardinality :db.cardinality/many}
                        :civ/peeps {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}})

        world-db (db/db-bulk-insert db [world])]
    world-db))

(def world-db-indexed
  (let [db (d/empty-db {:area {:db/index true}
                        :land/history {:db/valueType :db.type/ref
                                       :db/cardinality :db.cardinality/many}
                        :land/resources {:db/valueType :db.type/ref
                                         :db/cardinality :db.cardinality/many}
                        :land/units {:db/valueType :db.type/ref
                                     :db/cardinality :db.cardinality/many}
                        :land/civs {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}
                        :civ/territory {:db/valueType :db.type/ref
                                        :db/cardinality :db.cardinality/many}
                        :civ/peeps {:db/valueType :db.type/ref
                                    :db/cardinality :db.cardinality/many}})

        world-db (db/db-bulk-insert db [world])]
    world-db))

(comment
  (let [times 1000
        get-areas (fn [world-db]
                    (d/datoms world-db :avet :area))
        area ((comp #(nth % 2) first) (get-areas world-db-indexed))
        lookup-fn (fn [world-db area]
                    (d/datoms world-db :avet :area area))
        lookup-area (fn [world-db area attrs]
                      (reduce
                        (fn [v datom]
                          (let [eid (first datom)]
                            (cond-> (d/entity world-db eid)
                              attrs
                              (select-keys attrs))))
                        []
                        (d/datoms world-db :avet :area area)))]
    #_(db/q '[:find [(pull ?e [*]) ...] :where [?e :area ?a]] world-db)
    (time
      (dotimes [_ times]
        (db/q '[:find [(pull ?e [*]) ...] :where [?e :area ?a] :in $ ?a] world-db-unindexed area)
        #_(data/land-area world-db-unindexed area)))
    (time
      (dotimes [_ times]
        (data/land-area world-db-indexed area)))
    #_
    (time
      (dotimes [_ times]
        (lookup-fn world-db-unindexed area)))
    (time
      (dotimes [_ times]
        (lookup-fn world-db-indexed area)))
    (time
      (dotimes [_ times]
        (lookup-area world-db-indexed area nil)))
    #_(let [world-db world-db-indexed
            attrs nil]
        (reduce
          (fn [v datom]
            (let [eid (first datom)]
              (cond-> (d/entity world-db eid)
                attrs
                (select-keys attrs))))
          []
          (d/datoms world-db :avet :area area)))
    #_(mapv first (get-areas world-db-indexed))
    #_(lookup-area world-db-indexed area nil)
    (println "baseline:")
    (time
      (dotimes [_ times]
        (data/land-area->civ world-db-indexed area)))

    #_
    (time
      (dotimes [_ times]
        (reduce
          (fn [v datom]
            (let [eid (first datom)]
              (if-let [result (db/q '[:find [(pull ?c [:fruit-economy.civ/name {:civ/territory [*]}]) ...]
                                      :where
                                      [?c :civ/territory ?e]
                                      :in $ ?e]
                                world-db-indexed eid)]
                (into v result)
                v)
              #_(d/datoms world-db :eavt eid :civ/territory)))
          []
          (get-areas world-db-indexed))))
    (time
      (dotimes [_ times]
        (let [world-db world-db-indexed
              attrs nil]
          (reduce
            (fn [v datom]
              (let [eid (first datom)]
                (if-let [result (d/datoms world-db :avet :civ/territory eid)]
                  (into v
                    (map
                      (fn [datom]
                        (let [eid (first datom)]
                          (cond-> (d/entity world-db eid)
                            attrs
                            (select-keys attrs)))))
                    result)
                  v)))
            []
            (get-areas world-db)))))

    (time
      (dotimes [_ times]
        (let [world-db world-db-indexed
              attrs nil]
          (reduce
            (fn [v datom]
              (let [eid (first datom)]
                (if-let [result (d/datoms world-db :avet :civ/territory eid)]
                  (into v
                    (d/pull-many world-db
                      '[* {:civ/territory [*] :civ/peeps [*]}]
                      (mapv first result))
                    #_(map
                        (fn [datom]
                          (let [eid (first datom)]
                            (cond-> (d/pull world-db eid)
                              attrs
                              (select-keys attrs))))))
                  v)))
            []
            (get-areas world-db)))))))


#_(db/q '[:find (pull ?c [* {:civ/territory [*] :civ/peeps [*]}]) . :where [?e :area ?a] [?c :civ/territory ?e] :in $ ?a] world-db area)

(def *render-cache (atom {}))
(defn memoize-last [ctor]
  (let [*atom (volatile! nil)]
    (fn [& args']
      (or
        (when-some [[args value] @*atom]
          (println "IN memoize-last" args args' (= args args'))
          (if (= args args')
            value
            (when (instance? AutoCloseable value)
              (.close ^AutoCloseable value))))
        (let [value' (apply ctor args')]
          (vreset! *atom [args' value'])
          value')))))

(comment
  (require '[clojure.java.io :as io])
  (import '[io.github.humbleui.skija Surface Canvas Color4f FontMgr FontStyle Typeface Font Paint])
  (import '[io.github.humbleui.types IPoint IRect Rect])

  (identity @*render-cache)
  (reset! *render-cache {})

  (let [world-db world-db-indexed
        buffer (Surface/makeRasterN32Premul 640 360)
        canvas (.getCanvas buffer)
        buffer2 (Surface/makeRasterN32Premul 640 360)
        canvas2 (.getCanvas buffer2)
        cell 22
        [camera-x camera-y] [10 12]
        make-rendered (fn [cell] (let [buffer (Surface/makeRasterN32Premul cell cell #_#_(* cell 10) (* cell 10))]
                                   {:buffer buffer
                                    :canvas (.getCanvas buffer)
                                    :image (.makeImageSnapshot buffer)}))]
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (doseq [tickable (data/tickable world-db)]
      (let [args (:area tickable)
            render-fn (fn [x y]
                        (let [rendered (get-in @*render-cache [args :rendered] (make-rendered cell))
                              offset-x (+ (* x cell) camera-x)
                              offset-y (+ (* y cell) camera-y)]
                          (println "IN INPUTS-FN!!" x y)
                          (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF3333CC)))]
                            #_(.drawRect canvas2 (Rect/makeXYWH (+ (* x cell) camera-x) (+ (* y cell) camera-y) cell cell) fill)
                            (.drawRect (:canvas rendered) (Rect/makeXYWH 0 0 #_#_offset-x offset-y cell cell #_#_(* cell 10) (* cell 10)) fill))
                          #_(assoc rendered :image (.makeImageSnapshot buffer))
                          #_{:canvas canvas2}
                          (assoc rendered :offset-x offset-x :offset-y offset-y :image (.makeImageSnapshot (:buffer rendered)))))
            inputs-fn (get-in @*render-cache [args :fn] (memoize-last render-fn))
            rendered' (apply inputs-fn args)
            rendered (get-in @*render-cache [args :rendered])]
        (println (:offset-x rendered') (:offset-y rendered') :rendered rendered :rendered' rendered')
        (when-not #_(identical? rendered rendered')
          (identical? (:canvas rendered) (:canvas rendered'))
          (swap! *render-cache assoc args {:fn inputs-fn
                                           :rendered rendered'}))
        ;(.drawImage canvas (:image rendered') (first args) (second args))
        (.drawImage canvas (:image rendered') #_(.makeImageSnapshot (:buffer rendered')) #_#_0 0 (:offset-x rendered') (:offset-y rendered'))))
    (io/copy
      (-> buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png")))

  (let [zoom 1.0
        init-cell 30
        cell 22
        content-width 600 content-height 400
        [min-width min-height] [600 400]
        [*canvas-width* *canvas-height*] [2400 1200]


        world-db world-db-indexed
        buffer (Surface/makeRasterN32Premul content-width content-height)
        canvas (.getCanvas buffer)
        [camera-x camera-y] [10 12]
        make-rendered (fn [cell] (let [buffer (Surface/makeRasterN32Premul cell cell #_#_(* cell 10) (* cell 10))]
                                   {:buffer buffer
                                    :canvas (.getCanvas buffer)
                                    :image (.makeImageSnapshot buffer)}))


        scale (max (float (/ *canvas-width* content-width)) (float (/ *canvas-height* content-height)))
        x-scale (float (/ *canvas-width* content-width))
        y-scale (float (/ *canvas-height* content-height))

        ;; we're calling long because drawRect appears to draw at whole numbers so if we want no gaps,
        ;;   we should pass it whole numbers
        ;; TODO: need better names here, we want to be able to disambiguate the size of the tile, whether it's scaled or not and what those things depend on.
        cell' (long (/ (* init-cell zoom) scale))
        canvas-width' (long (* x-scale *canvas-width*))
        canvas-height' (long (* y-scale *canvas-height*))
        viewport-width' (inc (quot canvas-width' cell'))
        viewport-height' (inc (quot canvas-height' cell'))
        half-vw' (quot viewport-width' 2)
        half-vh' (quot viewport-height' 2)
        _ (println :cell cell' :scale scale :x-scale x-scale :y-scale y-scale)
        _ (println :width content-width :height content-height)
        _ (println :*canvas-width* *canvas-width* :*canvas-height* *canvas-height*)
        _ (println :canvas-width canvas-width' :canvas-height canvas-height')
        _ (println :viewport-width viewport-width' :viewport-height viewport-height')
        _ (println :half-vw half-vw' :half-vh half-vh')]
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (doseq [;; TODO: Figure out why this viewport-width fixed the tile rendering bug,
            ;;   it's almost certainly a scaling value thing.
            x (range viewport-width')
            y (range (quot viewport-height' 2))]
      (let [loc-x (+ (int (- x half-vw')) camera-x)
            loc-y (+ (int (- y half-vh')) camera-y)
            loc [loc-x loc-y]
            path [loc-y loc-x]
            #_#_#_#_
            tile (get-in terrain path)
            territory? (contains? territory loc)
            #_#_
            _ (println :x x :y y :loc-x loc-x :loc-y loc-y)]
        (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF3333CC)))]
          (.drawRect canvas #_(:canvas rendered) (Rect/makeXYWH x y #_#_offset-x offset-y (quot cell 2) (quot cell 2) #_#_(* cell 10) (* cell 10)) fill))))
    #_(doseq [tickable (data/tickable world-db)]
        (let [args (:area tickable)
              render-fn (fn [x y]
                          (let [rendered (get-in @*render-cache [args :rendered] (make-rendered cell))
                                offset-x (+ (* x cell) camera-x)
                                offset-y (+ (* y cell) camera-y)]
                            (println "IN INPUTS-FN!!" x y)
                            (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF3333CC)))]
                              #_(.drawRect canvas2 (Rect/makeXYWH (+ (* x cell) camera-x) (+ (* y cell) camera-y) cell cell) fill)
                              (.drawRect (:canvas rendered) (Rect/makeXYWH 0 0 #_#_offset-x offset-y cell cell #_#_(* cell 10) (* cell 10)) fill))
                            #_(assoc rendered :image (.makeImageSnapshot buffer))
                            #_{:canvas canvas2}
                            (assoc rendered :offset-x offset-x :offset-y offset-y :image (.makeImageSnapshot (:buffer rendered)))))
              inputs-fn (get-in @*render-cache [args :fn] (memoize-last render-fn))
              rendered' (apply inputs-fn args)
              rendered (get-in @*render-cache [args :rendered])]
          (println (:offset-x rendered') (:offset-y rendered') :rendered rendered :rendered' rendered')
          (when-not #_(identical? rendered rendered')
            (identical? (:canvas rendered) (:canvas rendered'))
            (swap! *render-cache assoc args {:fn inputs-fn
                                             :rendered rendered'}))
          ;(.drawImage canvas (:image rendered') (first args) (second args))
          (.drawImage canvas (:image rendered') #_(.makeImageSnapshot (:buffer rendered')) #_#_0 0 (:offset-x rendered') (:offset-y rendered'))))
    (io/copy
      (-> buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png")))



  (let [zoom 1.0
        init-cell 20
        cell init-cell

        ;; size of the window
        window-width 640 window-height 480
        window-rect (Rect/makeXYWH 0 0 window-width window-height)

        ;; size of the window's content
        content-width 640 content-height 420
        content-rect (Rect/makeXYWH 0 0 content-width content-height)

        ;; the size of the canvas within the window's content
        viewport-width 600  viewport-height 400
        viewport-rect (Rect/makeXYWH 0 0 viewport-width viewport-height)

        world-width (quot viewport-width cell) world-height (quot viewport-height cell)


        [min-width min-height] [600 400]

        world-db world-db-indexed
        territory (into #{} (map :area) (data/land-claims world-db))
        terrain (db/q '[:find ?v . :where [?e :fruit-economy.land/terrain ?v]] world-db)


        buffer (Surface/makeRasterN32Premul window-width window-height)
        canvas (.getCanvas buffer)
        [camera-x camera-y] [10 12]
        make-rendered (fn [cell] (let [buffer (Surface/makeRasterN32Premul cell cell #_#_(* cell 10) (* cell 10))]
                                   {:buffer buffer
                                    :canvas (.getCanvas buffer)
                                    :image (.makeImageSnapshot buffer)}))

        tile-color (unchecked-int 0xFFCC33CC)]
    (println (pr-str territory))
    (println (pr-str terrain))
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (with-open [w-fill (doto (Paint.) (.setColor (unchecked-int 0xFFCC3333)))
                c-fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))
                v-fill (doto (Paint.) (.setColor (unchecked-int 0xFF3333CC)))]
      (.drawRect canvas window-rect w-fill)
      (.drawRect canvas content-rect c-fill)
      (.drawRect canvas viewport-rect v-fill))

    (doseq [x (range world-width)
            y (range world-height)
            :let [;; pixel-x and pixel-y
                  px-x (+ (* x cell) 0) px-y (+ (* y cell) 0)

                  loc-x x ;(+ (int (- x half-vw)) camera-x)
                  loc-y y ;(+ (int (- y half-vh)) camera-y)
                  loc [loc-x loc-y]
                  path [loc-y loc-x]
                  biome (get-in terrain path)

                  _ (println :x x :y y :loc-x loc-x :loc-y loc-y :px-x px-x :px-y px-y)

                  tile-colour (land/render-tile-colour biome)

                  #_#_{::civ/keys [symbol tint] :as civ} (data/land-area->civ world-db loc [::civ/symbol ::civ/tint])]]
      (with-open [fill (doto (Paint.) (.setColor tile-colour))]
        (.drawRect canvas #_(:canvas rendered) (Rect/makeXYWH px-x px-y #_#_offset-x offset-y cell cell #_#_(quot cell 2) (quot cell 2) #_#_(* cell 10) (* cell 10)) fill)))

    (io/copy
      (-> buffer
        (.makeImageSnapshot)
        (.encodeToData)
        (.getBytes))
      (io/file "resources/mock-screen.png"))))