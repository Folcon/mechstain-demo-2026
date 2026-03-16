(ns fruit-economy.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [environ.core :refer [env]]
   [clojure.stacktrace :as stacktrace]
   [io.github.humbleui.app :as app]
   [io.github.humbleui.canvas :as canvas]
   [io.github.humbleui.core :as hui]
   [io.github.humbleui.paint :as paint]
   [io.github.humbleui.profile :as profile]
   [io.github.humbleui.window :as window]
   [io.github.humbleui.ui :as ui]
   [fruit-economy.state :as state]
   [fruit-economy.clock :as clock]
   [fruit-economy.humble-ui :as custom-ui]
   [fruit-economy.components :as cui]
   [fruit-economy.colour :refer [colour]]
   [fruit-economy.input :refer [mouse-button->kw key->kw]]
   [fruit-economy.graph :refer [graph?]]
   [fruit-economy.db.core :as db]
   [fruit-economy.data.core :as data]
   [fruit-economy.land :as land]
   [fruit-economy.unit :as unit]
   [fruit-economy.civ :as civ]
   [fruit-economy.game :as game]
   [fruit-economy.economy :as economy]
   [fruit-economy.civ-actions :as civ-actions]
   [fruit-economy.sim.core :as sim]
   [fruit-economy.jcomponent.chart :as chart]
   [fruit-economy.jchart :as jchart]
   [fruit-economy.sim.basic :as basic]
   [fruit-economy.ui.bits :as ui.bits :refer [padding show-map-ui]]
   [fruit-economy.ui.parts :as ui.parts]
   [fruit-economy.ui.controls :refer [on-key-pressed-impl]]
   [fruit-economy.ui.views :as ui.views]
   [fruit-economy.ui.screens :as ui.screens]
   [fruit-economy.screen-ui :as screen-ui]
   [fruit-economy.utils :refer [suppress-print]]
   [datascript.core :as d]
   [taoensso.tufte :as tufte]
   [taoensso.timbre :refer [set-level! log]]
   [clj-async-profiler.core :as prof])
  (:import
   [io.github.humbleui.jwm EventMouseButton EventMouseMove EventMouseScroll EventKey KeyModifier]
   [io.github.humbleui.skija Surface Canvas Color4f FontMgr FontStyle Typeface Font Paint PaintMode]
   [io.github.humbleui.types IPoint IRect Rect])
  (:gen-class))


(set! *warn-on-reflection* true)

(defn debug? [] (= (env :debug?) "true"))

;; TODO: Make this compile time using env vars for prod builds
(when-not (debug?)
  (set-level! :warn))

(when (debug?)
  (let [instrument (requiring-resolve 'clojure.spec.test.alpha/instrument)]
    (instrument 'fruit-economy.sim.market/load-order)))

(defonce stats-accumulator
  (tufte/add-accumulating-handler! {:ns-pattern "*"}))

(defonce stats-drainer
  ;; Will drain and print formatted stats every minute
  (future
    (while (debug?)
      (when-let [m (not-empty @stats-accumulator)]
        (println (tufte/format-grouped-pstats m)))
      (Thread/sleep 60000))))

(defonce font-mgr (FontMgr/getDefault))

(defn init-world [world-name width height]
  (-> (land/make-land world-name width height)
    (land/gen-land)
    (land/populate 50 #_100)
    (unit/spawn-units 10)
    (economy/add-resources)
    (civ/try-spawn-new-civs 10)))

;; GAME STATE
(defn new-state []
  (let [width 120
        height 80
        width 60
        height 40
        init-cell 30
        world (init-world "World" width height)]
    {:width width
     :height height
     :canvas-width state/*canvas-width*
     :canvas-height state/*canvas-height*
     :camera [(/ width 2) (/ height 2)]
     :peep [5 5]
     :init-cell init-cell
     :cell init-cell
     :zoom 1.
     :svg-xyz [0 0 0.]

     :world world
     :world-db (db/db-bulk-insert (db/init-db) [world])
     :history-index 0
     :civ-index 0

     :economy? false
     :info? false
     :paused? false
     :tick 0
     :tick-ms 5000
     :last-tick (System/currentTimeMillis)
     :render-ms 400
     :last-render (System/currentTimeMillis)

     :map-view :default-map-view}))

(when (nil? @state/*state)
  (reset! state/*state (new-state)))
(defonce *render-cache (atom {}))
;; END GAME STATE

(defonce ^Typeface face-default
  (.matchFamiliesStyle ^FontMgr font-mgr (into-array String [#_"Menlo" "JetBrains Mono" #_"Fira Code"] #_[".SF NS", "Helvetica Neue", "Arial"]) FontStyle/NORMAL))

(defonce game-glyph
  (let [economy-glyph (rand-nth ["💰" "💸" "🤑" "🏦" "💵" "💱" "💴" "💶" "💷"])
        fruit-glyph (rand-nth ["🥭" "🍅" "🍊" "🍉" "🍏" "🍐" "🍌" "🍑" "🍈" "🍋" "🍍" "🍓" "🍎" "🍇" "🥝" "🍒"])]
    (str fruit-glyph economy-glyph)))

(defonce emoji-glyph "🍀")
(defonce ^Typeface emoji-face (.matchFamilyStyleCharacter ^FontMgr font-mgr nil FontStyle/NORMAL nil (.codePointAt ^String emoji-glyph 0)))

(defonce *clicks (atom 0))
#_
(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-default (Font. face-default (float (* 13 scale)))
          leading (.getCapHeight (.getMetrics font-default))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          fill-button-normal (doto (Paint.) (.setColor (unchecked-int 0xFFade8f4)))
          fill-button-hovered (doto (Paint.) (.setColor (unchecked-int 0xFFcaf0f8)))
          fill-button-active (doto (Paint.) (.setColor (unchecked-int 0xFF48cae4)))]
      (ui/column
        (ui/row
          (ui/label "Top Bar" {:font font-default :paint fill-text}))
        (ui/row
          (ui/valign 0.1
            (ui/column
              (ui/label "Left Sidebar" {:font font-default :paint fill-text})))
          (ui/valign 0.5
            (ui/halign 0.4
              (ui/column
                (ui/label "Hello from Humble UI! 👋" {:font font-default :paint fill-text})
                (ui/gap 0 leading)
                (ui/dynamic _ [clicks @*clicks]
                  (ui/label (str "Clicked: " clicks) {:font font-default :paint fill-text}))
                (ui/gap 0 leading)
                (ui/clickable
                  #(swap! *clicks inc)
                  (ui/clip-rrect (* scale 4)
                    (ui/dynamic ctx [active?  (:hui/active? ctx)
                                     hovered? (:hui/hovered? ctx)]
                      (let [[label fill] (cond
                                           active?  ["Active"    fill-button-active]
                                           hovered? ["Hovered"   fill-button-hovered]
                                           :else    ["Unpressed" fill-button-normal])]
                        (ui/fill fill
                          (ui/padding (* scale 20) leading
                            (ui/label label {:font font-default :paint fill-text}))))))))))
          (ui/valign 0.1
            (ui/halign 1.2
              (ui/column
                (ui/label "Right Sidebar" {:font font-default :paint fill-text}))))
          ;; Not currently visible, should work out what the layout system is
          (ui/row
            (ui/label "Bottom Bar" {:font font-default :paint fill-text})))))))

(defn on-tick [state now]
  (let [{:keys [tick world-db]} state
        world-db' (game/on-tick world-db)
        tick' (inc tick)]
    (assoc state
      :world-db world-db'
      :tick tick'
      :last-tick now)))

(defn on-render [state now]
  (assoc state :last-render now))

(comment
  (let [[x y] [21 47]
        loc [x y]
        path [y x]]
    (->
      (get-in @state/*state [:world ::land/civ-name->civ #_:terrain "Civ A+0"])
      ;(get-in @state/*state [:world ::land/terrain y x])
      #_keys)),)

(comment

  (.offset (Rect/makeXYWH 30 20 18 8) 10 10)

  (let [cell 10
        window-width 60
        window-height 40
        camera-x (quot window-width 2)
        camera-y (quot window-height 2)
        width (quot window-width cell)
        height (quot window-height cell)
        w (quot width 2)
        h (quot height 2)
        zoom 1
        vs (vec
             (for [x (range width)
                   y (range height)
                   :let [[cx cy] [(+ camera-x x) (+ camera-y y)]
                         #_#__ (when (or (< cx (+ camera-x (* x 2))))
                                 (println [[x y] [cx cy]]))
                         xz (* (- x w) zoom)
                         yz (* (- y h) zoom)
                         cxz (+ camera-x xz) cyz (+ camera-y yz)]]
               [[x y] [cx cy] :! [xz yz] :> [cxz cyz]]))]
    {:vs vs
     :width width
     :height height}))
(def *render-cache (atom {}))

(defn make-rendered [cell]
  (let [buffer (Surface/makeRasterN32Premul cell cell #_#_(* cell 10) (* cell 10))]
    {:buffer buffer
     :canvas (.getCanvas buffer)
     :image (.makeImageSnapshot buffer)}))

(defn draw-impl [^Canvas canvas viewport-width viewport-height]
  (let [{:keys [camera peep world world-db zoom cell hovering viewport-width viewport-height half-vw half-vh tick paused? tick-ms last-tick render-ms last-render] :as state} @state/*state

        font-default (Font. face-default (float (* 24 zoom)))
        fill-default (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))

        ;; Rendering text
        font-bounds (.measureText font-default "X")
        font-offset-x (-> (- (.getLeft font-bounds))
                        (- (/ (- (.getWidth font-bounds) cell) 2)))
        font-offset-y (-> (- (.getTop font-bounds))
                        (- (/ (- (.getHeight font-bounds) cell) 2)))


        ;; Rendering emoji
        emoji-font (Font. emoji-face (float (* 20 zoom)))
        emoji-bounds (.measureText emoji-font emoji-glyph)
        emoji-offset-x (-> (- (.getLeft emoji-bounds))
                         (- (/ (- (.getWidth emoji-bounds) cell) 2)))
        emoji-offset-y (-> (- (.getTop emoji-bounds))
                         (- (/ (- (.getHeight emoji-bounds) cell) 2)))

        {::land/keys [terrain area->civ-name civ-name->civ area->resources area->units]} (data/land-data world-db)
        territory (into #{} (map :area) (data/land-claims world-db))
        [camera-x camera-y] camera
        now (System/currentTimeMillis)
        render? (> (- now last-render) render-ms)]
    ;; slowing render down
    (when render?
      (swap! state/*state on-render now))

    (.clear canvas (unchecked-int 0xFFFFFBBB))

    #_(println :panel tick)

    ;; walk cells eq to window size
    ;; TODO: This approach is workable, but not quite good enough, I need to be able to cache at multiple levels.
    ;;   So for example if there's no civ or thing changes, don't re-render.
    (doseq [x (range viewport-width)
            y (range viewport-height)]
      (let [;; args are the args captured in our memoize-last
            args [world-db camera x y]
            render-fn (fn [world-db camera x y]
                        (let [rendered (get-in @*render-cache [args :rendered] (make-rendered cell))
                              ;; pixel-x and pixel-y
                              px-x (* x cell) px-y (* y cell)

                              loc-x (+ (int (- x half-vw)) camera-x)
                              loc-y (+ (int (- y half-vh)) camera-y)
                              loc [loc-x loc-y]
                              path [loc-y loc-x]
                              biome (get-in terrain path)

                              territory? (contains? territory loc)
                              {::civ/keys [symbol tint] :as civ} (when territory? (data/land-area->civ world-db loc))

                              things (data/land-area world-db loc)
                              size (count things)
                              thing (when-not (zero? size) (:glyph (nth things (rem tick size))))

                              [glyph tile-colour font dx dy] (cond
                                                               thing [thing (if territory? tint (land/render-tile-colour biome)) emoji-font emoji-offset-x emoji-offset-y]
                                                               civ [symbol tint font-default font-offset-x font-offset-y]
                                                               territory? ["" tint font-default font-offset-x font-offset-y]
                                                               :else ["" (land/render-tile-colour biome) font-default font-offset-x font-offset-y])]
                          (with-open [fill (doto (Paint.) (.setColor tile-colour))]
                            (.drawRect ^Canvas (:canvas rendered) (Rect/makeXYWH 0 0 cell cell) fill)
                            (.drawString ^Canvas (:canvas rendered) glyph dx dy font fill-default))
                          (assoc rendered :px-x px-x :px-y px-y :image (.makeImageSnapshot ^Surface (:buffer rendered)))))
            inputs-fn (get-in @*render-cache [args :fn] (hui/memoize-last render-fn))
            rendered' (apply inputs-fn args)
            rendered (get-in @*render-cache [args :rendered])]
        (when-not (identical? rendered rendered')
          (swap! *render-cache assoc args {:fn inputs-fn
                                           :rendered rendered'}))
        (.drawImage canvas (:image rendered') (:px-x rendered') (:px-y rendered'))))

    (when hovering
      (with-open [fill (doto (Paint.) (.setColor (colour 0x66FFD700)))]
        (.drawRect canvas (Rect/makeXYWH ((comp #(* (quot % cell) cell) first :screen) hovering) ((comp #(* (quot % cell) cell) second :screen) hovering) cell cell) fill)))

    (with-open [fill (doto (Paint.) (.setColor (unchecked-int 0xFF33CC33)))]
      (.drawRect canvas (Rect/makeXYWH (first peep) (second peep) 10 10) fill))))

(defn draw-mini-panel-impl
  "Render for small panel, this will be a sort of global render
  context to ensure stuff like ticks still keep happening"
  [^Canvas canvas window-width window-height]
  (let [{:keys [tick tick-ms last-tick paused?] :as _state} @state/*state
        now (System/currentTimeMillis)
        fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))

        cell-x (/ window-width 2)
        cell-y (/ window-height 2)

        ;; Rendering emoji
        emoji-font (Font. emoji-face (float 72))
        emoji-bounds (.measureText emoji-font emoji-glyph)
        emoji-offset-x (-> (- (.getLeft emoji-bounds))
                         (- (/ (- (.getWidth emoji-bounds) cell-x) 2)))
        emoji-offset-y (-> (- (.getTop emoji-bounds))
                         (- (/ (- (.getHeight emoji-bounds) cell-y) 2)))]
    ;; tick handling
    (when (and
            (not paused?)
            (> (- now last-tick) tick-ms))
      (swap! state/*state on-tick now))

    #_(println :mini-panel tick)

    ;; Put the clear here to show where the mini panel is,
    ;;   will probably use it in some other way later
    (.clear canvas (unchecked-int 0xFFFFFBBB))
    (.drawString canvas game-glyph emoji-offset-x (+ emoji-offset-y (/ cell-y 2)) emoji-font fill-text)))

(declare on-resize)
(comment
  #_(db/db-bulk-insert (db/init-db) [(init-world "World" 20 20)])
  (db/q '[:find [?value ...] :where [?e :land/civs ?value]] (:world-db @state/*state))

  (let [world-db (:world-db @state/*state)
        civ-name "Shoff"
        attrs {::civ/power #()}]
    (db/q '[:find (pull ?e [:db/id]) . :where [?e ::civ/name ?civ-name] :in $ ?civ-name] world-db civ-name))

  (let [world-db (:world-db @state/*state)]
    (db/q '[:find [?e ?value] :where [?e :fruit-economy.land/economy ?value]] world-db))

  (let [world-db (:world-db @state/*state)]
    (data/upsert-land-data world-db (civ/try-spawn-new-civs (data/land-data world-db) 1))

    #_(db/q '[:find [?e ?value] :where [?e :fruit-economy.land/area->units ?value]] world-db))

  (require '[fruit-economy.sim.core :as sim])

  (let [world-db (:world-db @state/*state)
        ;_ (mapv (comp datascript.core/touch (partial datascript.core/entity world-db)) (db/q '[:find [?value ...] :where [?e :land/civs ?civ-id] [?civ-id :civ/peeps ?value]] world-db))]
        world-db' world-db                                  ;(game/on-tick world-db)
        world-db'' world-db                                 ;(game/on-tick world-db')
        touch-civ (fn [id] (data/entity world-db'' '[* {:civ/territory [*] :civ/peeps [*]}] id))
        civs (mapv touch-civ (data/ordered-civs world-db''))]
    civs
    #_(civ/try-spawn-new-civs (data/land-data world-db') 1))

  (let [world-db (:world-db @state/*state)
        {:keys [world-db subordinate-choose subordinate planned-decision]} v
        civ-name "Ohiû"
        #_#_#_#_{old-civ-id :db/id :as old-civ} (data/civ-name->civ world-db existing-claim [{:civ/territory '[*]}])
                {new-civ-id :db/id :as new-civ} (data/civ-name->civ world-db civ-name [{:civ/territory '[*]}])

        {new-civ-id :db/id :as new-civ} (data/civ-name->civ world-db civ-name [{:civ/territory '[*]}])
        target (:target subordinate-choose)
        area->claim-ids (into {} (map (juxt :area :db/id)) (data/land-claims world-db))
        existing-claim-id (get area->claim-ids target)]
    #_(if existing-claim-id
        (let [{old-civ-id :db/id :as old-civ} (data/land-claims->civ world-db existing-claim-id)]
          [[:db/retract old-civ-id :civ/territory existing-claim-id]
           [:db/add new-civ-id :civ/territory existing-claim-id]])
        [{:db/id new-civ-id :civ/territory {:area target :development 0}}])
    #_(db/db-bulk-insert
        (db/db-bulk-insert
          (db/db-bulk-insert
            (db/init-db) [{:db/id -1 :name :a :civ/territory {:area [0 0]}}
                          {:db/id -2 :name :b}])
          [{:db/id 1 :civ/territory {:area [2 0]}}])
        [#_[:db/retractEntity 2]
         [:db/retract 1 :civ/territory 2]
         [:db/add 3 :civ/territory 2]])
    ;(into {} (map (juxt :area #(select-keys % [:name :kind :glyph])) (data/land-resources world-db)))
    ;(into #{} (map :area) (data/land-resources world-db))
    ;(tap> (data/land-data world-db))
    ;(sim/choose world-db subordinate planned-decision)
    (sim/process-decision world-db subordinate-choose))

  (let [world-db (:world-db @state/*state)
        ;_ (mapv (comp datascript.core/touch (partial datascript.core/entity world-db)) (db/q '[:find [?value ...] :where [?e :land/civs ?civ-id] [?civ-id :civ/peeps ?value]] world-db))
        units (data/on-tick world-db)
        ent (first units)
        decide sim/decide
        decision (decide nil ent)
        civ-name (:fruit-economy.civ/name ent)]
    (as-> (let [leader ent
                {:civ/keys [peeps]} (db/q '[:find (pull ?e [{:civ/peeps [*]}]) . :where [?e :civ/peeps ?peep-id] :in $ ?peep-id] world-db (:db/id leader))]
            (reduce
              (fn [v peep]
                (conj v {:db/id (:db/id peep) :planned-decision (decide world-db leader)}))
              []
              peeps)) $
      (data/update-ticked world-db $)
      (let [leader ent
            subordinate (db/q '[:find (pull ?peep-id [*]) . :where [?e :civ/peeps ?peep-id] :in $ ?peep-id] $ (:db/id leader))
            subordinate-choose (sim/choose $ subordinate (:planned-decision subordinate))]
        (def v {:world-db $ :subordinate-choose subordinate-choose :subordinate subordinate :planned-decision (:planned-decision subordinate)})
        (sim/process-decision $ subordinate-choose))
      #_(db/q '[:find (pull ?e [*]) . :where [?e :fruit-economy.civ/name ?civ-name] [?e :fruit-economy.civ/tech-level] :in $ ?civ-name] $ civ-name))

    #_(let [{::civ/keys [peeps] :as civ} (get civ-name->civ civ-name)]
        (reduce-kv
          (fn [land peep-name peep]
            (let [decision (decide land-data peep)]
              (assoc-in land [::land/civ-name->civ civ-name ::civ/peeps peep-name :planned-decision] decision)))
          land-data
          peeps))
    ;((:on-tick ent) ent world-db)
    ;((:on-tick ent) ent (data/land-data world-db))
    #_(db/q '[:find (pull ?e [*]) :where [?e :on-tick]] world-db)
    #_(into {} (db/q '[:find ?a (pull ?r [*]) :where [?e :land/resources ?r] [?r :area ?a]] world-db)))

  (let [world-db (:world-db @state/*state)
        civ-name "Pìlthoìc"]
    (->> (data/civ-name->civ world-db civ-name '[:civ/territory {:civ/territory [:area]}])
      vals
      (first)
      (mapv :area))
    #_(db/q '[:find [(pull ?v [*]) ...] :where [?e :fruit-economy.civ/name ?civ-name] [?e :civ/territory ?v] :in $ ?civ-name] world-db civ-name)
    #_(game/unit-tick world-db)
    #_(let [tickable (data/on-tick world-db)
            _ (println :tickable tickable)
            ticked (into [] (comp (map (fn [{:keys [on-tick] :as unit}] (println :ticking unit) (on-tick unit world-db))) #_cat) tickable)]
        ticked #_(data/update-ticked world-db ticked)))


  (do
    (reset! state/*state (new-state))
    nil
    #_(on-resize @state/*window))


  (let [world-db (:world-db @state/*state)
        [id history] (db/q '[:find [?e ?value] :where [?e ::land/history ?value]] world-db)]
    (->> (db/db-bulk-insert world-db
           [{:db/id id ::land/history (conj history "TEST")}])
      (db/q '[:find (pull ?e [:db/id ::land/history]) :where [?e ::land/history ?value]])))

  (let [world-db (:world-db @state/*state)
        civ-name "Shoff"
        {power ::civ/power} (db/q '[:find (pull ?e ?attrs) . :where [?e ::civ/name ?civ-name] :in $ ?civ-name ?attrs] world-db civ-name [:db/id ::civ/power])]
    [(db/q '[:find (pull ?e ?attrs) . :where [?e ::civ/name ?civ-name] :in $ ?civ-name ?attrs]
       (data/upsert-civ world-db civ-name {:fruit-economy.civ/power (+ power (* power (/ 4 10)))})
       civ-name [:db/id ::civ/power])
     (db/q '[:find (pull ?e ?attrs) . :where [?e ::civ/name ?civ-name] :in $ ?civ-name ?attrs]
       (data/update-civ world-db civ-name {:fruit-economy.civ/power (partial + (* power (/ 4 10)))})
       civ-name [:db/id ::civ/power])])

  (let [world-db (:world-db @state/*state)
        civ-index 0
        ordered-civs (data/ordered-civs world-db)
        controlling-civ-id (nth ordered-civs civ-index)
        controlling-civ (data/entity world-db controlling-civ-id)
        _ (println ::controlling-civ controlling-civ)]
    (when controlling-civ
      (-> (civ-actions/grow-pop' world-db controlling-civ)
        #_(data/entity controlling-civ-id)))))

(defn on-key-pressed-mini-panel-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @state/*state]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (println :mini-panel raw-event))

    (when (= event :hui/mouse-button)
      (println :mini-panel raw-event))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :mini-panel key)
      (println (:peep @state/*state))
      (condp contains? key
        #{:q}
        (let [history-index (get state :history-index)
              history (data/history-log-entries (:world-db state))
              history-size (count history)]
          (swap! state/*state assoc :history-index (min (dec history-size) (inc history-index))))

        #{:e}
        (let [history-index (get state :history-index)]
          (swap! state/*state assoc :history-index (max 0 (dec history-index))))

        #{:t}
        (swap! state/*state update :economy? not)

        #{:y}
        (swap! state/*state update :world-db data/step-economy)

        #{:p}
        (swap! state/*state update :paused? not)

        #{:close-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! state/*state assoc :civ-index (rem (inc civ-index) size)))

        #{:open-bracket}
        (let [civ-index (get state :civ-index)
              size (data/civ-count (:world-db state))]
          (swap! state/*state assoc :civ-index (rem (+ (dec civ-index) size) size)))

        ;#{:digit5}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)
        ;
        ;      ordered-civs (data/ordered-civs (:world-db state))
        ;      controlling-civ-id (nth ordered-civs civ-index)
        ;      controlling-civ' (data/entity (:world-db state) controlling-civ-id)]
        ;  (when controlling-civ
        ;    (-> state/*state
        ;      (swap! update :world civ-actions/grow-pop controlling-civ)
        ;      (swap! update :world-db civ-actions/grow-pop' controlling-civ'))))
        ;
        ;#{:digit6}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)
        ;
        ;      ordered-civs (data/ordered-civs (:world-db state))
        ;      _ (println :ordered-civs ordered-civs)
        ;      controlling-civ-id (nth ordered-civs civ-index)
        ;      _ (println :controlling-civ-id controlling-civ-id)
        ;      controlling-civ' (data/entity (:world-db state) controlling-civ-id)
        ;      _ (println :controlling-civ controlling-civ)]
        ;  (when controlling-civ
        ;    (swap! state/*state update :world civ-actions/expand-territory controlling-civ)))
        ;
        ;#{:digit7}
        ;(let [civ-index (get state :civ-index)
        ;      civ-name->civ (get-in state [:world ::land/civ-name->civ])
        ;      controlling-civ (nth (vals civ-name->civ) civ-index)]
        ;  (when controlling-civ
        ;    (swap! state/*state update :world civ-actions/improve-tech-level controlling-civ)))

        #{:r}
        (do
          (reset! state/*state (new-state))
          (on-resize @state/*window))

        ;; (println :mini-panel key)
        nil))))

(defn on-key-pressed-svg-impl [{:keys [key pressed? event] :as raw-event}]
  (let [state @state/*state]

    ;; mouse
    #_#_
    (when (and (= event :hui/mouse-move) (:hui.event/pos raw-event))
      (println :tech-ui-panel raw-event))

    (when (= event :hui/mouse-button)
      (println :tech-ui-panel raw-event))

    ;; keyboard
    (when (and (= event :key) pressed?)
      (println :tech-ui-panel key)
      (condp contains? key
        #{:d :right}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 0] + 20))

        #{:a :left}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 0] - 20))

        #{:s :down}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 1] + 20))

        #{:w :up}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 1] - 20))

        #{:minus}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 2] - 0.1))

        #{:equals}
        (let [svg-xyz (get state :svg-xyz)]
          (println :svg-xyz svg-xyz)
          (swap! state/*state update-in [:svg-xyz 2] + 0.1))

        ;; (println :tech-ui-panel key)
        nil))))

#_
(def app
  (ui/dynamic ctx [{:keys [scale bounds x-scale y-scale xy-scale]} ctx
                   {:keys [camera tick history-index civ-index economy? svg-xyz]} @state/*state
                   history (data/history-log-entries (get @state/*state :world-db))]
    (let [font-default (Font. face-default (float (* 18 scale)))
          font-small (Font. face-default (float (* 12 scale)))
          fill-text (doto (Paint.) (.setColor (unchecked-int 0xFF000000)))
          history-size (count history)
          {::land/keys [civ-name->civ economy]} (data/land-data (get @state/*state :world-db))
          controlling (nth (keys civ-name->civ) civ-index)
          [svg-x svg-y svg-z] svg-xyz
          canvas-width (* x-scale state/*canvas-width*)
          canvas-height (* y-scale state/*canvas-height*)]
      (ui/row
        (ui/column
          (custom-ui/ui-canvas 150 150 {:on-paint #'draw-mini-panel-impl
                                        :on-event #'on-key-pressed-mini-panel-impl}))
        (ui/valign 0.5
          (ui/halign 0.5
            (ui/column
              (if (zero? history-size)
                (ui/gap 0 0)
                (ui/padding 10
                  (ui/label (str (inc history-index) " of " history-size ": " (nth history (- (dec history-size) history-index))) {:font font-default :paint fill-text})))
              (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                (ui/padding 3
                  (if (and (graph? economy) economy?)
                    (ui/valign 0.5
                      (ui/halign 0.5
                        (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                          (ui/on-key-down on-key-pressed-svg-impl
                            (custom-ui/svg-canvas (economy/->svg economy))))))
                    (custom-ui/ui-canvas canvas-width canvas-height
                      {:on-paint #'draw-impl
                       :on-event #'on-key-pressed-impl}))))
              (ui/padding 10
                (ui/label (str (inc history-index) " of " history-size ": " (nth history (- (dec history-size) history-index))) {:font font-default :paint fill-text}))
              (ui/fill (doto (Paint.) (.setColor (unchecked-int 0xFFFFFFFF)))
                (ui/padding 3
                  (if (and (graph? economy) economy?)
                    (ui/valign 0.5
                      (ui/halign 0.5
                        (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                          (ui/on-key-down on-key-pressed-svg-impl
                            (custom-ui/svg-canvas (economy/->svg economy))))))
                    (custom-ui/ui-canvas canvas-width canvas-height
                      {:on-paint #'draw-impl
                       :on-event #'on-key-pressed-impl}))))
              (ui/padding 10
                (ui/label (str "👋🌲🌳Camera: " (pr-str camera) " Year: " tick (when controlling (str " controlling " controlling))) {:font font-default :paint fill-text}))
              (ui/padding 10
                (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") {:font font-small :paint fill-text}))
              (ui/padding 10
                (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") {:font font-small :paint fill-text})))))))))

(def economy-ui-view
  (ui/on-key-down (juxt on-key-pressed-svg-impl on-key-pressed-mini-panel-impl)
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [world-db svg-xyz]} @state/*state]
        (let [{::land/keys [economy]} (data/land-data world-db)
              [svg-x svg-y svg-z] svg-xyz]
          (ui/column
            ui.parts/top-bar-ui
            (ui/gap 0 padding)
            [:stretch 1
             (ui/row
               [:stretch 1
                (ui/valign 0.5
                  (ui/halign 0.5
                    ;; TODO: Current scroll / zoom broken on svg
                    (ui/with-context {:svg-x svg-x :svg-y svg-y :svg-z svg-z :paint (doto (Paint.) (.setColor (unchecked-int 0xFFEEEE00)))}
                      (custom-ui/svg-canvas (economy/->svg economy)))))])]))))))

(def world-map
  (ui/dynamic ctx [{:keys [scale x-scale y-scale font-default emoji-font font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black]} ctx
                   {:keys [world-db tick camera zoom map-view] :as state} @state/*state]
    (let [{::land/keys [terrain temp elev raw-resource units area->units] :as d} (data/land-data world-db)
          territory (into #{} (map :area) (data/land-claims world-db))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)

          [left right top bottom] lrtb

          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x                         ;(+ (int (- x half-vw)) camera-x)
                            loc-y y                         ;(+ (int (- y half-vh)) camera-y)
                            loc [loc-x loc-y]
                            path [loc-y loc-x]

                            biome (get-in terrain path)

                            ;; temp + elevation map
                            ;; Distribute out food for foraging + rock for crafting
                            local-temp (get-in temp path)
                            local-elev (get-in elev path)
                            raw-resource (get-in raw-resource path)

                            local-temp' (if local-temp (int (quot (* (+ local-temp 1) 255) 2)) 0)
                            local-elev' (if local-elev (int (quot (* (+ local-elev 1) 255) 2)) 0)

                            territory? (contains? territory loc)
                            {::civ/keys [symbol tint] :as civ} (when territory? (data/land-area->civ world-db loc))

                            things (data/land-area world-db [x y])
                            size (count things)
                            thing (when-not (zero? size) (:glyph (nth things (rem tick size))))

                            default-tint (condp = map-view
                                           :temp-view (colour local-temp' 0 0)
                                           :elev-view (colour 0 local-elev' 0)
                                           :climate-view (colour local-temp' local-elev' 0)
                                           :forage-view (colour 0 (* (get raw-resource :food 0) 40) 0)
                                           :mine-view (colour 0 (* (get raw-resource :rock 0) 40) 0)
                                           :default-map-view (land/render-tile-colour biome)
                                           (colour 0 0 0))]
                        (cond
                          thing [thing (if territory? tint (land/render-tile-colour biome)) emoji-font emoji-offset-x emoji-offset-y]
                          civ [symbol tint map-font font-offset-x font-offset-y]
                          territory? ["" tint map-font font-offset-x font-offset-y]
                          :else ["" default-tint map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [_ (when hovered?
                                (swap! state/*state assoc :hover-loc [x-idx y-idx]))
                            [glyph tile-colour font] (unit-data x-idx y-idx)]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (paint/fill tile-colour))
                          (ui/width cell
                            (ui/halign #_(/ _dx 30) 0.5
                              (ui/height cell
                                (ui/valign #_(/ _dy 30) 0.5
                                  (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def map-ui-view
  (ui/on-key-down (juxt on-key-pressed-impl on-key-pressed-mini-panel-impl)
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale font-small fill-white fill-black fill-green fill-dark-gray fill-light-gray]} ctx
                       {:keys [camera tick]} @state/*state]
        (ui/column
          ui.parts/top-bar-ui
          (ui/gap 0 padding)
          [:stretch 1 nil]
          world-map
          (ui/row
            (ui/clickable
              #(reset! state/*state (new-state))
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "RESET!" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state on-tick (System/currentTimeMillis))
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Tick" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :temp-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Temp" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :elev-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Elev" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :climate-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Climate" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :forage-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Forage" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :mine-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Mine" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! state/*state assoc :map-view :default-map-view)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Map View" {:font font-small :paint fill-white})))))))
          (ui/padding 5
            (ui/label (str "👋🌲🌳Camera Center: " (pr-str camera) " Year: " tick #_(when controlling (str " controlling " controlling))) {:font font-small :paint fill-black}))
          (ui/padding 5
            (ui/label (str "[r]: Reset World, [t]: Swap between Map and Economy / Tech Tree, [y]: Evolve Economy / Tech Tree") {:font font-small :paint fill-black}))
          (ui/padding 5
            (ui/label (str "[WASD] or arrow keys: Pan the camera, [-]: Zoom Out, [+]: Zoom In") {:font font-small :paint fill-black})))))))

(def peep-ui-view
  (ui/on-key-down on-key-pressed-impl
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale font-small fill-black]} ctx
                       peeps (:peeps @state/*state)
                       buildings (:buildings @state/*state)
                       selected-building (:selected-building @state/*state)
                       power (:power @state/*state)]
        (ui/column
          ui.parts/top-bar-ui
          #_(ui/halign 0.5
              (ui/row
                (interpose (ui/gap padding 2)
                  (for [[name building] buildings
                        :let [used? (used? building)]]
                    (ui/fill (if used? fill-dark-gray fill-green)
                      (ui/padding 10
                        (let [display-fn (if-not used? (partial cui/radio-button state/*state name [:selected-building]) identity)]
                          (ui/column
                            (display-fn
                              (show-map-ui building {:font font-small :paint fill-black}))))))))))
          (ui/gap 0 padding)
          (show-map-ui (first (mapv #(data/entity (:world-db @state/*state) '[* {:civ/territory [*] :civ/peeps [*]}] %) (data/ordered-civs (:world-db @state/*state)))) font-small fill-black)
          #_(ui/halign 0.5
              (ui/row
                (interpose (ui/gap padding 2)
                  (for [[name peep] peeps
                        :let [alive? (alive? peep)]]
                    (ui/fill (if alive? fill-yellow fill-dark-gray)
                      (ui/padding 10
                        (let [display-fn (if-not alive? tombstone (partial checkbox [:peeps name :peep/selected]))]
                          (ui/column
                            (display-fn
                              (show-map-ui peep {:font font-small :paint fill-black}))))))))))
          #_(let [power? (> power 0)
                  active? (and selected-building power?)
                  btn-message (if power? (str "Activate " selected-building " with one Agent") (str "Your Agents are all busy, why not send some Peeps on a Quest?"))]
              (custom-ui/<>
                (when selected-building
                  (ui/gap 0 padding)
                  (ui/halign 0.5
                    (ui/padding 10 10
                      (ui/label btn-message {:font font-small :paint fill-black}))))
                (ui/gap 0 padding)
                (ui/halign 0.5
                  (ui/fill (if active? fill-green fill-dark-gray)
                    (ui/clickable
                      #(when active? (swap! state/*state process-building-activation))
                      (ui/padding 10 10
                        (ui/label "⇫ Act" {:font font-small :paint fill-white}))))))))))))

(defn things-coll [coll]
  (reduce
    (fn [m [[k v] idx]]
      (update m k (fnil conj []) (assoc v :id idx)))
    {}
    (map vector coll (range))))

(defn make-bug []
  {:glyph "🐞" :wealth 0 :vision (inc (rand-int 4)) :hunger (inc (rand-int 4))})

(defn gen-bug-world [size peep]
  {:space (apply vector
            (map (fn [_]
                   (apply vector (map (fn [_] (let [food #_(inc (rand-int 4)) (- 5 (int (Math/sqrt (rand-int 32))))] {:init-food food :food food :pher (rand-int 255) :max-age (+ (rand-int 5) 20)}))
                                   (range size))))
              (range size)))
   :units
   (things-coll
     (repeatedly peep (fn [] [[(rand-int size) (rand-int size)] (make-bug)])))})

(def bug-world-size 100 #_4 #_100)
(def bug-count 200 #_2 #_200)

(def *world
  (atom (gen-bug-world bug-world-size bug-count)))

(defn apply-cell [world f]
  (mapv
    (fn [row]
      (mapv f row))
    (:space world)))

(defn apply-unit [world f]
  (mapv
    (fn [row]
      (mapv f row))
    (:units world)))

(defn hunt [world]
  (reduce-kv
    (fn [world' loc units]
      (let [[x y] loc]
        (reduce
          (fn [w {:keys [vision] :as unit}]
            (let [sees (into []
                         cat
                         [(for [x (range (- x vision) (inc (+ x vision)))
                                :when (not= [x y] loc)]
                            [x y])
                          (for [y (range (- y vision) (inc (+ y vision)))
                                :when (not= [x y] loc)]
                            [x y])])
                  [best-food-loc _] (reduce
                                      (fn [[loc food] target]
                                        (let [[x y] target
                                              target-food (get-in w [:space y x :food] 0)]
                                          (if (> target-food food)
                                            [target target-food]
                                            [loc food])
                                          (if (> target-food food)
                                            [target target-food]
                                            [loc food])))
                                      [loc (get-in w [:space y x :food] 0)]
                                      sees)]
              (update-in w [:units best-food-loc] (fnil conj []) unit)))
          (update world' :units dissoc loc)
          units)))
    world
    (:units world)))

(defn mark-dead-bug [world loc idx]
  (let [loc-units (get-in world [:units loc] 0)
        food (get-in loc-units [idx :wealth])
        starving? (< food 0)]
    (if starving?
      (update world :dead (fnil conj []) [loc idx])
      world)))

(defn try-eat [world]
  (reduce-kv
    (fn [world' loc units]
      (let [[x y] loc]
        (reduce
          (fn [w [idx {:keys [hunger] :as unit}]]
            (let [food (get-in w [:space y x :food] 0)
                  gathered (min food (+ (quot food 2) 2))]
              (println #_(get w :units) :units (pr-str units))
              (println :gathered gathered :food food :take (+ (quot food 2) 2))
              (println [x y] idx (get-in w [:space y x]))
              (-> w
                (update-in [:space y x :food] - gathered)
                (update-in [:units loc idx :wealth] + gathered)
                (update-in [:units loc idx :wealth] - hunger)
                (mark-dead-bug loc idx))))
          world'
          (partition 2 (interleave (range) units)))))
    world
    (:units world)))

#_#_
(defn drop-nth [n coll]
  (if (vector? coll)
    (into (subvec coll 0 n) (subvec coll (inc n)))
    (into (empty coll) (keep-indexed #(if (not= %1 n) %2)) coll)))

(defn clear-dead-bug [world loc idx]
  (let [loc-units (get-in world [:units loc] 0)
        food (get-in loc-units [idx :wealth])
        starving? (< food 0)]
    (cond
      (and starving? (== (count loc-units) 1))
      (update world :units dissoc loc)

      starving?
      (update-in world [:units loc] #(drop-nth idx %))

      :else world)))

(defn clear-dead-bugs [world]
  (println :dead (:dead world))
  (let [deletions (apply
                    merge-with into
                    (into []
                      (map (fn [[k v]] {k [v]}))
                      (:dead world)))]
    (reduce-kv
      (fn [w loc idxs]
        (let [bugs (get-in w [:units loc])]
          (println :dead? loc idxs bugs)
          (println :cleared? (-> w
                               (update-in [:units loc] (partial into [] (remove (fn [{:keys [wealth]}] (> 0 wealth)))))
                               (get-in [:units loc])))
          (if (= (count bugs) (count idxs))
            (update w :units dissoc loc)
            (update-in w [:units loc] (partial into [] (remove (fn [{:keys [wealth]}] (> 0 wealth))))))))
      (dissoc world :dead)
      deletions)))

(comment
  (let [units {[43 29] [{:glyph "🐞", :wealth 9, :vision 2, :hunger 2, :id 162}], [0 77] [{:glyph "🐞", :wealth 7, :vision 2, :hunger 2, :id 170}], [71 85] [{:glyph "🐞", :wealth 9, :vision 2, :hunger 2, :id 187}], [22 69] [{:glyph "🐞", :wealth 0, :vision 2, :hunger 4, :id 150}], [2 52] [{:glyph "🐞", :wealth 6, :vision 2, :hunger 1, :id 117}], [41 63] [{:glyph "🐞", :wealth 3, :vision 3, :hunger 3, :id 137}], [96 93] [{:glyph "🐞", :wealth 5, :vision 3, :hunger 3, :id 0}], [30 15] [{:glyph "🐞", :wealth 5, :vision 2, :hunger 2, :id 20}], [37 9] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 168}], [42 75] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 67}], [5 46] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 166} {:glyph "🐞", :wealth 6, :vision 4, :hunger 2, :id 142} {:glyph "🐞", :wealth 4, :vision 3, :hunger 2, :id 107}], [43 27] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 103}], [81 64] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 179}], [65 11] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 146}], [38 74] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 122}], [67 51] [{:glyph "🐞", :wealth 11, :vision 2, :hunger 1, :id 154}], [9 18] [{:glyph "🐞", :wealth 7, :vision 4, :hunger 2, :id 76}], [3 9] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 56}], [51 57] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 10}], [75 2] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 186}], [44 65] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 147}], [9 76] [{:glyph "🐞", :wealth 9, :vision 1, :hunger 1, :id 49}], [2 92] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 23}], [9 43] [{:glyph "🐞", :wealth 6, :vision 1, :hunger 2, :id 44}], [10 8] [{:glyph "🐞", :wealth 8, :vision 1, :hunger 2, :id 126}], [52 55] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 50}], [73 80] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 34}], [60 38] [{:glyph "🐞", :wealth 11, :vision 2, :hunger 1, :id 124}], [43 94] [{:glyph "🐞", :wealth 9, :vision 3, :hunger 1, :id 121}], [94 4] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 139}], [65 91] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 71}], [70 23] [{:glyph "🐞", :wealth 0, :vision 2, :hunger 4, :id 194}], [33 26] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 42}], [80 46] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 91}], [82 53] [{:glyph "🐞", :wealth 9, :vision 2, :hunger 1, :id 129}], [80 96] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 112}], [48 21] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 28}], [22 31] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 21}], [17 81] [{:glyph "🐞", :wealth 7, :vision 1, :hunger 2, :id 13}], [70 12] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 115}], [81 30] [{:glyph "🐞", :wealth 8, :vision 3, :hunger 2, :id 86}], [45 62] [{:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :id 78}], [62 71] [{:glyph "🐞", :wealth 2, :vision 3, :hunger 3, :id 116}], [71 31] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 114}], [21 78] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 174}], [88 26] [{:glyph "🐞", :wealth 11, :vision 1, :hunger 1, :id 95}], [22 13] [{:glyph "🐞", :wealth 0, :vision 2, :hunger 4, :id 60}], [1 74] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 51}], [65 7] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 185}], [2 30] [{:glyph "🐞", :wealth 0, :vision 2, :hunger 3, :id 88}], [35 66] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 171}], [87 61] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 198}], [2 20] [{:glyph "🐞", :wealth 3, :vision 4, :hunger 3, :id 94}], [13 97] [{:glyph "🐞", :wealth 3, :vision 3, :hunger 3, :id 172} {:glyph "🐞", :wealth 3, :vision 3, :hunger 1, :id 163}], [57 72] [{:glyph "🐞", :wealth 11, :vision 1, :hunger 1, :id 125}], [35 21] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 79}], [80 10] [{:glyph "🐞", :wealth 2, :vision 3, :hunger 3, :id 136}], [43 12] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 81}], [31 24] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 140}], [22 59] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 111}], [68 93] [{:glyph "🐞", :wealth 12, :vision 1, :hunger 1, :id 100}], [95 10] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 54}], [44 76] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 175}], [35 86] [{:glyph "🐞", :wealth 7, :vision 3, :hunger 2, :id 63}], [53 47] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 149}], [23 14] [{:glyph "🐞", :wealth 7, :vision 1, :hunger 2, :id 144}], [98 29] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 22}], [20 55] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 7}], [25 46] [{:glyph "🐞", :wealth 0, :vision 2, :hunger 4, :id 48}], [8 75] [{:glyph "🐞", :wealth 3, :vision 4, :hunger 3, :id 158}], [82 97] [{:glyph "🐞", :wealth 12, :vision 2, :hunger 1, :id 55}], [45 86] [{:glyph "🐞", :wealth 2, :vision 1, :hunger 3, :id 85}], [2 66] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 104}], [6 29] [{:glyph "🐞", :wealth 3, :vision 3, :hunger 3, :id 132}], [35 1] [{:glyph "🐞", :wealth 7, :vision 3, :hunger 2, :id 17}], [32 76] [{:glyph "🐞", :wealth 5, :vision 4, :hunger 2, :id 165}], [79 54] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 128}], [37 14] [{:glyph "🐞", :wealth 4, :vision 4, :hunger 3, :id 99}], [65 58] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 108}], [47 69] [{:glyph "🐞", :wealth 5, :vision 1, :hunger 2, :id 123} {:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 15}], [12 48] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 27}], [76 67] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 77}], [20 39] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 84}], [47 40] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 120}], [6 56] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 5}], [89 49] [{:glyph "🐞", :wealth 3, :vision 1, :hunger 3, :id 127}], [66 25] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 31}], [36 49] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 191}], [20 52] [{:glyph "🐞", :wealth 10, :vision 2, :hunger 1, :id 181}], [16 72] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 180}], [37 46] [{:glyph "🐞", :wealth 6, :vision 3, :hunger 2, :id 195}], [59 45] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 82}], [7 38] [{:glyph "🐞", :wealth 6, :vision 3, :hunger 2, :id 46}], [19 14] [{:glyph "🐞", :wealth 7, :vision 2, :hunger 2, :id 64}], [70 5] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 92}], [76 62] [{:glyph "🐞", :wealth 0, :vision 1, :hunger 4, :id 52}], [19 50] [{:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :id 133}], [14 44] [{:glyph "🐞", :wealth 7, :vision 4, :hunger 2, :id 183}], [49 57] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 113}], [6 50] [{:glyph "🐞", :wealth 5, :vision 2, :hunger 2, :id 61}], [37 3] [{:glyph "🐞", :wealth 12, :vision 2, :hunger 1, :id 182}], [8 91] [{:glyph "🐞", :wealth 10, :vision 1, :hunger 1, :id 39}], [39 81] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 1}], [25 92] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 101}], [71 72] [{:glyph "🐞", :wealth 3, :vision 1, :hunger 2, :id 59}], [30 32] [{:glyph "🐞", :wealth 8, :vision 3, :hunger 2, :id 89}], [8 86] [{:glyph "🐞", :wealth 1, :vision 1, :hunger 3, :id 53}], [69 99] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 177}], [16 56] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 70}], [58 29] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 193}], [51 35] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 155}], [89 99] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 109}], [39 68] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 43}], [15 56] [{:glyph "🐞", :wealth 3, :vision 1, :hunger 2, :id 41}], [16 29] [{:glyph "🐞", :wealth 3, :vision 4, :hunger 3, :id 19}], [0 98] [{:glyph "🐞", :wealth 6, :vision 4, :hunger 2, :id 35}], [60 25] [{:glyph "🐞", :wealth 6, :vision 1, :hunger 2, :id 151}], [27 90] [{:glyph "🐞", :wealth 12, :vision 2, :hunger 1, :id 9}], [3 44] [{:glyph "🐞", :wealth 5, :vision 1, :hunger 2, :id 164}], [12 79] [{:glyph "🐞", :wealth 8, :vision 2, :hunger 2, :id 157}], [94 80] [{:glyph "🐞", :wealth 12, :vision 2, :hunger 1, :id 148}], [38 34] [{:glyph "🐞", :wealth 1, :vision 1, :hunger 2, :id 6}], [7 5] [{:glyph "🐞", :wealth 11, :vision 2, :hunger 1, :id 176} {:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 30}], [90 32] [{:glyph "🐞", :wealth 3, :vision 2, :hunger 3, :id 97}], [50 40] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 47}], [44 42] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 173}], [32 11] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 135}], [43 2] [{:glyph "🐞", :wealth 1, :vision 1, :hunger 3, :id 134}], [35 95] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 74}], [4 34] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 106}], [3 87] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 90}], [5 94] [{:glyph "🐞", :wealth 4, :vision 4, :hunger 3, :id 156}], [22 4] [{:glyph "🐞", :wealth 10, :vision 1, :hunger 1, :id 98}], [8 52] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 199}], [76 19] [{:glyph "🐞", :wealth 0, :vision 3, :hunger 4, :id 69}], [61 20] [{:glyph "🐞", :wealth 7, :vision 1, :hunger 2, :id 3}], [53 91] [{:glyph "🐞", :wealth 8, :vision 1, :hunger 1, :id 66}], [19 2] [{:glyph "🐞", :wealth 4, :vision 4, :hunger 3, :id 143}], [5 83] [{:glyph "🐞", :wealth 12, :vision 4, :hunger 1, :id 161}], [73 91] [{:glyph "🐞", :wealth 4, :vision 2, :hunger 3, :id 45}], [49 69] [{:glyph "🐞", :wealth 11, :vision 1, :hunger 1, :id 73}], [60 14] [{:glyph "🐞", :wealth 12, :vision 2, :hunger 1, :id 169}], [53 24] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 178}], [66 24] [{:glyph "🐞", :wealth 0, :vision 4, :hunger 4, :id 16}], [66 21] [{:glyph "🐞", :wealth 12, :vision 3, :hunger 1, :id 4}], [20 6] [{:glyph "🐞", :wealth 6, :vision 1, :hunger 2, :id 24}], [68 81] [{:glyph "🐞", :wealth 11, :vision 3, :hunger 1, :id 83}], [17 20] [{:glyph "🐞", :wealth 8, :vision 4, :hunger 2, :id 33}], [4 93] [{:glyph "🐞", :wealth 4, :vision 3, :hunger 3, :id 14}]}
        loc [5 46]
        idx 3
        gathered 0]
    (get-in units [loc])))

(defn reset-food [world]
  (update world :space
    #(into [] (map (fn [row]
                     (mapv
                       (fn [cell]
                         (assoc cell :food (:init-food cell)))
                       row))) %)))

(defn grow-food [world]
  (update world :space
    #(into [] (map (fn [row]
                     (mapv
                       (fn [{:keys [food init-food] :as cell}]
                         (assoc cell :food (min (inc food) init-food)))
                       row))) %)))

(defn age [world]
  (reduce-kv
    (fn [world' loc units]
      (let [[x y] loc]
        (reduce
          (fn [w {:keys [max-age] :as unit}]
            (update-in w [:units loc] (fnil conj []) unit))
          units)))
    world
    (:units world)))

(defn tick-bug-world [{:keys [on-tick-fns] :or {on-tick-fns []} :as world}]
  (reduce
    (fn [w on-tick]
      (on-tick w))
    world
    on-tick-fns))

(comment
  (swap! *world tick-bug-world))

(comment

  (things-coll
    (repeatedly 2 (fn [] [[(rand-int 1) (rand-int 1)] {:wealth 0 :glyph "🐞"}])))

  (swap! *world tick-bug-world)

  (let [world @*world]
    #_(try-eat world)
    #_(clear-dead-bugs world)
    (mapv
      (fn [row]
        (mapv
          (fn [cell]
            (println cell)
            cell)
          row))
      (:space world)))

  (do
    (swap! *world
      #(mapv
         (fn [row]
           (mapv
             (fn [cell]
               cell)
             row))
         %))
    nil))

(def land-map
  (ui/dynamic ctx [{:keys [scale x-scale y-scale font-default emoji-font font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black]} ctx
                   {:keys [tick camera zoom] :as state} @state/*state
                   {:keys [space units]} @*world]
    (let [canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)

          [left right top bottom] lrtb

          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x                         ;(+ (int (- x half-vw)) camera-x)
                            loc-y y                         ;(+ (int (- y half-vh)) camera-y)
                            loc [loc-x loc-y]
                            path [loc-y loc-x]

                            tile (get-in space path)

                            things (get units loc)
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))]
                        (when thing
                          (println :wealth thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          :else ["" (colour 0 (* (get tile :food 0) 40) 0 #_(get tile :pher 0)) map-font font-offset-x font-offset-y])))]
      (ui/column
        (interpose (ui/gap 0 0)
          (for [y-idx (range top bottom)]
            (ui/row
              (interpose (ui/gap 0 0)
                (for [x-idx (range left right)]
                  (ui/hoverable
                    (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                      (let [_ (when hovered?
                                (swap! state/*state assoc :hover-loc [x-idx y-idx]))
                            [glyph tile-colour font] (unit-data x-idx y-idx)]
                        (ui/fill (if hovered?
                                   (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                   (paint/fill tile-colour))
                          (ui/width cell
                            (ui/halign #_(/ _dx 30) 0.5
                              (ui/height cell
                                (ui/valign #_(/ _dy 30) 0.5
                                  (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def data-viz
  (ui/dynamic ctx [{:keys [scale x-scale y-scale font-default font-small emoji-font font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black]} ctx
                   {:keys [tick camera zoom] :as state} @state/*state
                   {:keys [space units]} @*world]
    (let [canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)

          [left right top bottom] lrtb

          map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. emoji-face (float (* scale 8 zoom)))

          unit-data (fn [x y]
                      (let [;; pixel-x and pixel-y
                            loc-x x                         ;(+ (int (- x half-vw)) camera-x)
                            loc-y y                         ;(+ (int (- y half-vh)) camera-y)
                            loc [loc-x loc-y]
                            path [loc-y loc-x]

                            tile (get-in space path)

                            things (get units loc)
                            size (count things)
                            {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))]
                        (when thing
                          (println :wealth thing (get thing :wealth)))
                        (cond
                          thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                          :else ["" (colour 0 (* (get tile :food 0) 40) 0 #_(get tile :pher 0)) map-font font-offset-x font-offset-y])))

          wealth-freqs (vec (into (sorted-map) (frequencies (into [] (comp cat (map :wealth)) (vals (:units @*world))))))
          size (count wealth-freqs)
          mx 30
          wealth-freqs-filtered (into [["$" "#"]]
                                  (if (< mx size)
                                    (into (subvec wealth-freqs 0 (- mx 3)) cat [[["" ""] ["..." (str (- size (dec mx)) "+")] ["" ""]] (subvec wealth-freqs (- size 2) size)])
                                    wealth-freqs))
          freq-avg-fn (fn [freq]
                        (let [[total size] (reduce (fn [v [val count]] (-> v (update 0 + (* val count)) (update 1 + count))) [0 0] freq)]
                          (float (/ total size))))]
      (ui/column
        #_(ui/padding 5
            (ui/label (pr-str units) {:font font-default :paint fill-black}))
        #_(ui/padding 5
            (show-map-ui {:wealth (into (sorted-map) (frequencies (into [] (comp cat (map :wealth)) (vals units))))} font-default fill-black))
        #_(ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (custom-ui/custom-ui 100 100
                {:on-paint #'draw-impl--})))
        (ui/row
          (ui/label (str "# " (reduce + (mapv second wealth-freqs))) {:font font-small :paint fill-black})
          (ui/gap padding 2)
          (ui/label (str "μ 👁: " (freq-avg-fn (frequencies (into [] (comp cat (map :vision)) (vals (:units @*world)))))) {:font font-small :paint fill-black})
          (ui/gap padding 2)
          (ui/label (str "μ 🍖: " (freq-avg-fn (frequencies (into [] (comp cat (map :hunger)) (vals (:units @*world)))))) {:font font-small :paint fill-black}))
        (ui/gap 2 padding)
        (ui/padding 5
          (ui/column
            (interpose (ui/gap padding 2)
              (for [entry wealth-freqs-filtered
                    :let [[_wealth quantity] entry]]
                (ui/row
                  (interpose (ui/gap padding 0)
                    (into
                      (if (number? quantity)
                        (custom-ui/<>
                          (ui/fill (paint/fill (colour 150 150 150))
                            (ui/gap (* quantity 2) 2)))
                        (custom-ui/<>
                          (ui/gap 0 0)))
                      (for [val entry]
                        (ui/width 20
                          (ui/label (str val) {:font font-small :paint fill-black}))))))))))
        #_(ui/column
            (interpose (ui/gap 0 0)
              (for [[wealth quantity] (into (sorted-map) (frequencies (into [] (comp cat (map :wealth)) (vals units))))]
                (ui/row
                  (ui/padding 10 0
                    (ui/width 28
                      (ui/label (format "%s: %s  " wealth quantity) {:font font-small :paint fill-black})))
                  (ui/gap 0 0)
                  (ui/fill (paint/fill (colour 150 150 150))
                    (ui/gap quantity 2))))))

        #_(interpose (ui/gap 0 0)
            (for [y-idx (range top bottom)]
              (ui/row
                (interpose (ui/gap 0 0)
                  (for [x-idx (range left right)]
                    (ui/hoverable
                      (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                        (let [_ (when hovered?
                                  (swap! state/*state assoc :hover-loc [x-idx y-idx]))
                              [glyph tile-colour font] (unit-data x-idx y-idx)]
                          (ui/fill (if hovered?
                                     (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                     (paint/fill tile-colour))
                            (ui/width cell
                              (ui/halign #_(/ _dx 30) 0.5
                                (ui/height cell
                                  (ui/valign #_(/ _dy 30) 0.5
                                    (ui/label glyph {:font font :paint fill-white}))))))))))))))))))

(def chart-ui-component
  (ui/dynamic ctx [{:keys [space units]} @*world]
    (let [wealth-freqs (into (sorted-map) (frequencies (into [] (comp cat (map :wealth)) (vals units))))
          data (chart/->dataset {"Wealth" wealth-freqs})]
      (ui/column
        (ui/row
          (ui/clip
            (ui/fill (paint/fill 0xFF90DC48)
              (ui/width 2
                (ui/height 100
                  (ui/fill (paint/fill 0xFF4890DC)
                    (chart/chart (chart/make-chart-fn data))))))))))))

(defn on-key-pressed-bug-impl [{event-type :hui/event :hui.event.key/keys [key pressed?] :as event}]
  (when (and (= event-type :hui/key-down) pressed?)
    (println :bug key)
    (condp contains? key
      #{:key/t}
      (swap! *world tick-bug-world)

      #{:key/r}
      (reset! *world (gen-bug-world bug-world-size bug-count))

      nil)))

(def on-tick-fns
  {#_#_"hunt try-eat clear-dead-bugs reset-food"
          [hunt
           try-eat
           clear-dead-bugs
           reset-food]
   "hunt try-eat clear-dead-bugs"
   [hunt
    try-eat
    clear-dead-bugs]
   "hunt try-eat clear-dead-bugs grow-food"
   [hunt
    try-eat
    clear-dead-bugs
    grow-food]
   "hunt try-eat age clear-dead-bugs grow-food"
   [hunt
    try-eat
    clear-dead-bugs
    grow-food]})

(def on-tick-fns-opts
  [["hunt" hunt]
   ["try-eat" try-eat]
   ["age" age]
   ["clear-dead-bugs" clear-dead-bugs]
   ["grow-food" grow-food]
   ["reset-food" reset-food]])

(def bug-ui-view
  (ui/on-key-down on-key-pressed-bug-impl
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [font-small fill-white fill-dark-gray fill-green]} ctx
                       {:keys [camera tick]} @state/*state]
        (ui/column
          ui.parts/top-bar-ui
          (ui/gap 0 padding)
          [:stretch 1 nil]
          (ui/row
            land-map
            (ui/gap 0 padding)
            data-viz)
          (ui/gap 0 padding)
          chart-ui-component
          (ui/gap 0 padding)
          (ui/row
            (ui/clickable
              #(reset! *world (gen-bug-world bug-world-size bug-count))
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "RESET!" {:font font-small :paint fill-white}))))))
            (ui/clickable
              #(swap! *world tick-bug-world)
              (ui/hoverable
                (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                  (ui/fill (if hovered? fill-green fill-dark-gray)
                    (ui/padding 10 10
                      (ui/label "Tick" {:font font-small :paint fill-white}))))))
            (interpose (ui/gap 0 0)
              (for [[label f] on-tick-fns]
                (ui/row
                  (ui/clickable
                    #(swap! *world assoc :on-tick-fns f)
                    (ui/hoverable
                      (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                        (ui/fill (if hovered? fill-green fill-dark-gray)
                          (ui/padding 10 10
                            (ui/label (str label) {:font font-small :paint fill-white})))))))))))))))

(def jpanel-ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-dark-gray fill-green fill-yellow]} ctx]
    (ui/column
      ui.parts/top-bar-ui
      (ui/gap 0 padding)
      [:stretch 1 nil]
      land-map
      #_jchart/jswing-test
      chart-ui-component
      (ui/row
        (ui/clickable
          #(reset! *world (gen-bug-world bug-world-size bug-count))
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "RESET!" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! *world tick-bug-world)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (if hovered? fill-green fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "Tick" {:font font-small :paint fill-white}))))))
        (ui/dynamic ctx [selected-on-tick-fns (get @*world :selected-on-tick-fns)]
          (ui/row
            (interpose (ui/gap 0 0)
              (for [[label _f] on-tick-fns-opts]
                (ui/row
                  (ui/clickable
                    #(doto *world
                       (swap! update-in [:selected-on-tick-fns label] not)
                       (swap! assoc :on-tick-fns (into [] (comp (filter (fn [[k v]] (contains? (:selected-on-tick-fns @*world) k))) (map second)) on-tick-fns-opts)))
                    (ui/hoverable
                      (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                        (let [colour (cond
                                       hovered? fill-yellow
                                       (get selected-on-tick-fns label) fill-green
                                       :else fill-dark-gray)]
                          (ui/fill colour
                            (ui/padding 10 10
                              (ui/label (str label) {:font font-small :paint fill-white}))))))))))))))))

(defn label [text]
  (ui/fill (paint/fill 0xFFB2D7FE)
    (ui/halign 0.5
      (ui/valign 0.5
        (ui/padding 10 10
          (ui/label text {:font-ui (Font. ^Typeface face-default (float 20)) :fill-text (paint/fill 0xFF000000)}))))))

(def align-ui-view
  (ui/valign 0.5
    (ui/row
      [:stretch 1 nil]
      [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                    (ui/column
                      [:stretch 1 (ui/halign 1 0 (label "Right to left (1 0)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0.5 0 (label "Center to left (0.5 0)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0.6 0.2 (label "Arbitrary (0.6 0.2)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0 (label "Left to left (0 0)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 1 0.5 (label "Right to center (1 0.5)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0.5 (label "Center to center (0.5 0.5)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0 0.5 (label "Left to center (0 0.5)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 1 1 (label "Right to right (1 1)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0.5 1 (label "Center to right (0.5 1)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (ui/halign 0 1 (label "Left to right (0 1)"))]
                      (ui/gap 0 1)
                      [:stretch 1 (label "Stretch")]))]
      [:stretch 1 nil]
      [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                    (ui/row
                      [:stretch 1 (ui/valign 1 0 (label "Bottom to top"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0.5 0 (label "Middle to top"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0 (label "Top to top"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 1 0.5 (label "Bottom to middle"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0.5 (label "Middle to middle"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0 0.5 (label "Top to middle"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 1 1 (label "Bottom to bottom"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0.5 1 (label "Middle to bottom"))]
                      (ui/gap 1 0)
                      [:stretch 1 (ui/valign 0 1 (label "Top to bottom"))]
                      (ui/gap 1 0)
                      [:stretch 1 (label "Stretch")]))]
      [:stretch 1 nil])))

(def v-view
  (chart/chart (chart/make-chart-fn2)))

(reset! state/*selected-ui-view (ffirst ui.views/ui-views))

(when (nil? @state/*floating)
  (reset! state/*floating false))

(add-watch state/*floating ::window
  (fn [_ _ _ floating]
    (when-some [window @state/*window]
      (if floating
        (window/set-z-order window :floating)
        (window/set-z-order window :normal)))))

(defn clock-tick-fn []
  (do
    (basic/do-tick-world)
    (window/request-frame @state/*window)))

(add-watch state/*menu ::speed
  (fn [_ _ {old-paused? :paused? :as old} {new-paused? :paused? :as new}]
    (when (not= old-paused? new-paused?)
      (if new-paused?
        (clock/stop-clock)
        (clock/start-clock clock-tick-fn)))))


(when (nil? @state/*menu)
  (reset! state/*menu (if (debug?) {:screen ui.screens/game-screen :started? true :paused? true :speed-ms 5000} {:screen ui.screens/start-screen :started? false :paused? true :speed-ms 5000})))

#_  ;; For debugging start-screen
(reset! state/*menu {:screen ui.screens/start-screen :started? false :paused? true :speed-ms 5000})
(reset! state/*menu {:screen ui.screens/game-screen :started? true :paused? true :speed-ms 5000})

#_
(reset! *menu
  {:speed-ms 5000
   :screen
   (ui/dynamic ctx [{:keys [scale x-scale y-scale
                            font-small fill-white fill-black fill-dark-gray fill-green fill-yellow
                            green-colour yellow-colour dark-gray-colour]} ctx]
     (ui/valign 0.5
       (ui/row
         [:stretch 1 nil]
         [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                       (ui/column
                         (ui/tooltip {:anchor :top-left :shackle :top-right
                                      :left -10 :up 0}
                           (ui/fill fill-dark-gray
                             (ui/row
                               (ui/width 120
                                 (ui/height 25
                                   (ui/label "TOOLTIP")))))
                           (ui/column
                             (ui/row
                               (ui/width 200
                                 (ui/height 300
                                   (ui/fill (paint/fill 0xFFEFFAFF)
                                     (ui/fill fill-yellow
                                       (ui/label "TOOLTIP TEST"))))))))))]
         [:stretch 1 nil]
         [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                       (ui/valign 0
                         (ui/halign 0
                           ;;(ui/width 200)
                           (ui/hoverable
                             (ui/dynamic ctx [{:keys [hui/active? hui/hovered?]} ctx]
                               (custom-ui/relative-rect {:anchor :top-left :shackle :top-right
                                                         :left -10 :up 0}
                                 (let [tip (ui/fill fill-dark-gray
                                             (ui/row
                                               (ui/width 120
                                                 (ui/height 25
                                                   (ui/label "TOOLTIP")))))
                                       tip' (cond
                                              active?  tip
                                              hovered? tip
                                              :else    (ui/gap 0 0))]
                                   tip')
                                 (ui/column
                                   (ui/row
                                     (ui/width 200
                                       (ui/height 300
                                         (ui/fill (paint/fill 0xFFEFFAFF)
                                           (ui/fill fill-yellow
                                             (ui/label "TOOLTIP TEST")))))))))))))]
         [:stretch 1 nil])))})


(def app
  (ui/dynamic ctx [scale (:scale ctx)]
    (let [font-ui   (Font. face-default (float (* 13 scale)))
          leading   (-> font-ui .getMetrics .getCapHeight Math/ceil (/ scale))
          fill-text (paint/fill 0xFF000000)
          selection-colour 0xFFB1D7FF
          white-colour 0xFFFFFFFF
          black-colour 0xFF000000
          light-gray-colour 0xFFD4D6DA
          dark-gray-colour 0xFF777C7E
          blue-colour 0xFFB2D7FE
          green-colour 0xFF6AAA64
          yellow-colour 0xFFC9B457
          red-colour 0xFFD53F3F]
      (ui/with-context {:face-default   face-default
                        :emoji-face     emoji-face
                        :game-glyph     game-glyph
                        :face-ui        face-default
                        :font-ui        font-ui
                        :leading        leading
                        :fill-text      fill-text
                        :fill-cursor    fill-text
                        :fill-selection (paint/fill selection-colour)
                        :selection-colour selection-colour

                        :font-default (Font. face-default (float (* 18 scale)))
                        :font-large (Font. ^Typeface face-default (float (* scale 26)))
                        :font-small (Font. ^Typeface face-default (float (* scale 13)))

                        :fill-white (paint/fill white-colour)
                        :fill-black (paint/fill black-colour)
                        :fill-light-gray (paint/fill light-gray-colour)
                        :fill-dark-gray (paint/fill dark-gray-colour)
                        :fill-blue (paint/fill blue-colour)
                        :fill-green (paint/fill green-colour)
                        :fill-yellow (paint/fill yellow-colour)
                        :fill-red (paint/fill red-colour)

                        :white-colour white-colour
                        :black-colour black-colour
                        :light-gray-colour light-gray-colour
                        :dark-gray-colour dark-gray-colour
                        :blue-colour blue-colour
                        :green-colour green-colour
                        :yellow-colour yellow-colour
                        :red-colour red-colour

                        :stroke-light-gray (paint/stroke 0xFFD4D6DA (* 2 scale))
                        :stroke-dark-gray (paint/stroke 0xFF777C7E (* 2 scale))}
        (ui/dynamic ctx [{:keys [screen]} @state/*menu]
          screen)))))

(comment
  (window/request-frame @state/*window))

(defn on-paint [window ^Canvas canvas]
  (canvas/clear canvas 0xFFF0F0F0)
  (let [bounds (window/content-rect window)
        {screen :work-area} (app/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))
        xy-scale (max x-scale y-scale)
        ctx {:bounds bounds :scale (window/scale window) :x-scale x-scale :y-scale y-scale :xy-scale xy-scale :ui-defer (atom [])}
        app app]
    (hui/draw app ctx (IRect/makeXYWH 0 0 (:width bounds) (:height bounds)) canvas)
    (when (seq @(:ui-defer ctx))
      (doseq [f @(:ui-defer ctx)]
        (f)))
    #_(window/request-frame window)))

(defn on-event [window event]
  (when-let [changed? (hui/event app event)]
    (window/request-frame window)))

(defn on-resize [window]
  (let [[min-width min-height] [600 400]
        [window-width window-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) (window/window-rect window))
        bounds (window/content-rect window)
        [content-width content-height] ((juxt #(.getWidth ^IRect %) #(.getHeight ^IRect %)) bounds)
        {:keys [init-cell zoom]} @state/*state
        scale (max (float (/ state/*canvas-width* content-width)) (float (/ state/*canvas-height* content-height)))
        {screen :work-area} (app/primary-screen)
        x-scale (float (/ (.getWidth ^IRect bounds) (.getWidth ^IRect screen)))
        y-scale (float (/ (.getHeight ^IRect bounds) (.getHeight ^IRect screen)))

        ;; we're calling long because drawRect appears to draw at whole numbers so if we want no gaps,
        ;;   we should pass it whole numbers
        ;; TODO: need better names here, we want to be able to disambiguate the size of the tile, whether it's scaled or not and what those things depend on.
        cell' (* init-cell zoom)
        canvas-width' (long (* x-scale state/*canvas-width*))
        canvas-height' (long (* y-scale state/*canvas-height*))
        viewport-width' (inc (quot canvas-width' cell'))
        viewport-height' (inc (quot canvas-height' cell'))
        half-vw' (quot viewport-width' 2)
        half-vh' (quot viewport-height' 2)]
    (println :cell' cell' :canvas-width canvas-width' :canvas-height' canvas-height' :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh')
    (swap! state/*state assoc
      :scale scale :cell cell' :canvas-width canvas-width' :canvas-height canvas-height' :content-width content-width :content-height content-height
      :viewport-width viewport-width' :viewport-height viewport-height' :half-vw half-vw' :half-vh half-vh')
    (window/set-window-size window (max window-width min-width) (max window-height min-height))))

(defn screen-sized-window [window {:keys [width height right y] :as _work-area}]
  (let [window-width width
        window-height height
        window-left (- right window-width)
        window-top (-> y
                     (+ height)
                     (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn small-window [window {:keys [width height right y] :as _work-area}]
  (let [window-width (/ width 2)
        window-height (/ height 2)
        window-left (- right window-width)
        window-top (-> y
                     (+ (/ height 2))
                     (- (/ window-height 2)))]
    (doto window
      (window/set-window-size window-width window-height)
      (window/set-window-position window-left window-top))))

(defn make-window []
  (let [{:keys [work-area]} (app/primary-screen)
        window-size-fn (if (debug?) small-window screen-sized-window)]
    (doto
      (window/make
        {:on-close (if (debug?) #(reset! state/*window nil) #(System/exit 0))
         :on-paint #'on-paint
         :on-event #'on-event
         :on-resize #'on-resize})
      (window/set-title "Fruit Economy 👋")
      (window-size-fn work-area)
      (window/set-visible true))))

#_(defn on-tick [state now]
    (let [{:keys [tick]} state
          tick' (inc tick)
          state' (assoc state
                   :tick tick'
                   :last-tick now)]
      (window/request-frame @state/*window)
      state'))

(defn tick-clock []
  (let [{:keys [tick-ms last-tick paused?]} @state/*state
        now (System/currentTimeMillis)]
    (when (and
            (not paused?)
            (> (- now last-tick) tick-ms))
      (swap! state/*state on-tick now))))


(defn -main [& args]
  ;; TODO: Display somewhere in the UI
  (println (str "VERSION: " (env :game-version) (when (debug?) "\nDEBUG BUILD")))
  (when (debug?)
    ;; Swap to require and resolve in one step!
    (future (apply (requiring-resolve 'nrepl.cmdline/-main) args)))
  (app/start #(reset! state/*window (make-window))))

;; Helps with REPL dev, on ns load forces a redraw
(some-> @state/*window window/request-frame)

(comment
  (do
    (app/doui (some-> @state/*window window/close))
    (reset! state/*window (app/doui (make-window))))

  (app/doui (window/set-z-order @state/*window :normal))
  (app/doui (window/set-z-order @state/*window :floating)))

(comment
  (-main)
  (identity @*clicks)
  (identity @state/*window)
  (dissoc (get-in @state/*state [:world #_::land/civ-name->civ]) ::land/elev ::land/elev-noise ::land/temp ::land/temp-noise)

  (let [civs (get-in @state/*state [:world #_::land/civ-name->civ #_::land/area->manor ::land/area->resources])]
    civs
    #_(into #{} (comp (map (fn [[_k {::civ/keys [territory]}]] territory)) cat) civs))

  (let [economy (get-in @state/*state [:world ::land/economy])]
    economy)

  (identity @fruit-economy.core/*clicks)

  (identity fruit-economy.core/app)

  (select-keys @state/*state [:viewport-width :viewport-height :cell])

  (use 'clojure.reflect 'clojure.pprint)
  (clojure.pprint/pprint (clojure.reflect/reflect fruit-economy.core/app))

  (swap! *clicks inc)

  (require '[clj-async-profiler.core :as prof])
  (prof/profile-for 60)
  (prof/profile {:return-file true}
    (basic/tick-world-1000x))
  (prof/serve-files 8080)

  (prof/profile
    (basic/tick-world-1000x))

  (let [interesting-flamegraph "/tmp/clj-async-profiler/results/01-cpu-flamegraph-2022-01-19-16-09-18.html"
        profiling-path (str/replace interesting-flamegraph "/tmp/" "resources/profiling/")]
    (io/make-parents profiling-path)
    (io/copy (io/file interesting-flamegraph) (io/file profiling-path)))

  (prof/find-profile "/tmp/clj-async-profiler/results/01_02-cpu-diff-2022-02-13-20-35-54.svg")

  (let [before-flamegraph 1 #_"/tmp/clj-async-profiler/results/02-cpu-flamegraph-2022-02-13-20-23-35.svg"
        after-flamegraph 2 #_"/tmp/clj-async-profiler/results/01-cpu-flamegraph-2022-02-13-19-52-24.svg"]
    (prof/generate-diffgraph before-flamegraph after-flamegraph {}))


  ;; Portal
  (do
    (def portal ((requiring-resolve 'portal.api/open)))
    (add-tap (requiring-resolve 'portal.api/submit)))

  (defn t> [args]
    (tap> args)
    args),)
