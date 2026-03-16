(ns fruit-economy.ui.views
  (:require [io.github.humbleui.ui :as ui]
            [io.github.humbleui.paint :as paint]
            [fruit-economy.state :as state]
            [fruit-economy.db.core :as db]
            [fruit-economy.data.core :as data]
            [fruit-economy.ui.bits :as ui.bits :refer [padding show-map-ui]]
            [fruit-economy.ui.parts :as ui.parts]
            [fruit-economy.ui.controls :refer [on-key-pressed-impl]]
            [fruit-economy.screen-ui :as screen-ui]
            [fruit-economy.sim.basic :as basic]
            [datascript.core :as d]
            [fruit-economy.sim.asami :as ad]
            [fruit-economy.colour :refer [colour colour-noise]]
            [fruit-economy.land :as land]
            [fruit-economy.humble-ui :as custom-ui]
            [ubergraph.core :as uber]
            [clojure.string :as str])
  (:import [io.github.humbleui.skija Font Typeface Paint]))


(defn market-label-fn [market]
  (println market)
  (condp = market :food "Food" :clothes "Clothes" :labour "Labour"))

(defn market-keys-fn [market key]
  (condp = [market key]
    [:food :producers] [:food-factory :farmer]
    [:food :demanders] [:peep]
    [:food :last-produced] :food/last-produced
    [:food :last-consumed] :food/last-consumed
    [:food :price] :food/price
    [:food :price-history] :food/price-history
    [:food :price-belief] :belief/food-price
    [:clothes :producers] [:clothes-factory]
    [:clothes :demanders] [:peep]
    [:clothes :last-produced] :clothes/last-produced
    [:clothes :last-consumed] :clothes/last-consumed
    [:clothes :price] :clothes/price
    [:clothes :price-history] :clothes/price-history
    [:clothes :price-belief] :belief/clothes-price
    [:labour :producers] [:peep]
    [:labour :demanders] [:food-factory :clothes-factory]
    [:labour :last-produced] :labour/last-produced
    [:labour :last-consumed] :labour/last-consumed
    [:labour :price] :labour/price
    [:labour :price-history] :labour/price-history
    [:labour :price-belief] :belief/labour-price))

(defn lookup-by-kind [world-db] (fn [kind] (data/lookup-avet world-db :kind kind)))

(def kind->emoji
  {:food-factory "🍲🏭" :clothes-factory "👚🏭" :peep "🧑"})

(defn show-peep [{:keys [health last-sold kind] :as peep}]
  (cond
    (= kind :peep)
    (str
      (cond
        (and (number? health) (zero? health)) "💀"
        (and (number? health) (> 5 health)) "🤕"
        :else "😀")
      (if (and (number? last-sold) (zero? last-sold))
        "🦥"
        "🐝"))
    (= kind :food-factory)
    (cond
      :else "🍲🏭")
    (= kind :clothes-factory)
    (cond
      :else "👚🏭")
    :else "👽"))

(def market-production-view
  (ui/dynamic ctx [{:keys [world-db cities selected-city selected-market fill-dark-gray fill-light-gray]} ctx]
     (let [city (db/entity world-db selected-city)

           producers (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :producers))
           demanders (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :demanders))

           label (market-label-fn selected-market)
           price ((market-keys-fn selected-market :price) city)
           price-history ((market-keys-fn selected-market :price-history) city)
           produced ((market-keys-fn selected-market :last-produced) city)
           consumed ((market-keys-fn selected-market :last-consumed) city)
           total-production (reduce (fn [v m] (+ v ((market-keys-fn selected-market :last-produced) m))) 0 cities)
           percent-of-production (if (zero? total-production) 0 (* (/ produced total-production) 100))]
       (ui/column
         (ui/row
           (ui/padding 20
             (ui/column
               (ui/label label)
               (ui/gap 0 5)
               (ui/label (clojure.pprint/cl-format nil "~,2f% of world production" percent-of-production))))
           (ui/with-context
             {:price price :price-history price-history}
             screen-ui/price-chart-ui))
         (ui/row
           (ui/padding 20
             (ui/column
               (ui/label "Produced by:")
               (ui/gap 0 4)
               (ui/height 100
                 (ui/row
                   (ui/vscrollbar
                     (ui/vscroll
                       (ui/column
                         (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                           (for [producer producers]
                             (ui/tooltip {:anchor :top-right :shackle :top-right}
                               (ui/label (pr-str producer))
                               (ui/fill fill-light-gray
                                 (ui/padding 150 20 150 20
                                   (ui/label (str ((:kind producer) kind->emoji) " " (pr-str (select-keys producer [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed]))))))))))))))
               (ui/padding 20
                 (ui/label (str "Total Produced: " produced)))))
           (ui/padding 20
             (ui/column
               (ui/label "Used by:")
               (ui/gap 0 4)
               (ui/height 100
                 (ui/row
                   (ui/vscrollbar
                     (ui/vscroll
                       (ui/column
                         (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                           (for [demander demanders]
                             (ui/tooltip {:anchor :top-right :shackle :top-right}
                               (ui/label (pr-str demander))
                               (ui/fill fill-light-gray
                                 (ui/padding 150 20 150 20
                                   (ui/label (str ((:kind demander) kind->emoji) " " (pr-str (select-keys demander [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed]))))))))))))))
               (ui/padding 20
                 (ui/label (str "Total Used: " consumed))))))

         #_#_#_
         (ui/label "Cities:")
         (ui/gap 0 2)
         (ui/height 100
           (ui/row
             (ui/vscrollbar
               (ui/vscroll
                 (ui/column
                   (interpose (ui/fill fill-dark-gray
                                (ui/gap 0 4))
                     (for [{:keys [settlement/name] :as market} cities]
                       (ui/column
                         (ui/label name)
                         (ui/gap 0 2)
                         ;;(ui/label (pr-str (db/touch market)))
                         ;;(ui/gap 0 2)
                         (show-map-ui market font-small fill-black)))))))))))))

(defn city-view [settlement]
  (ui/dynamic ctx [{:keys [font-small fill-black fill-red fill-green]} ctx
                   {:keys [world-db map-view]} @state/*world]
    (ui/column
      (ui/row
        (interpose (ui/gap 4 0)
          (for [k [:settlement/name :settlement/place]]
            (ui/label (str (get settlement k)) {:font font-small :paint fill-black}))))
      (ui/gap 0 4)
      (ui/row
        (ui/column
          (ui/label (str "Money " (:money (first (get settlement :_governs)))))
          (ui/gap 0 2)
          (ui/row
            [:stretch 4 (ui/label "Food")]
            [:stretch 2 (ui/label (str (get settlement :food/price) "g") {:paint (ui.bits/compare->fill settlement :food/last-demand :food/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :food/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil]
            [:stretch 4 (ui/label "Clothes")]
            [:stretch 2 (ui/label (str (get settlement :clothes/price) "g") {:paint (ui.bits/compare->fill settlement :clothes/last-demand :clothes/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :clothes/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil]
            [:stretch 4 (ui/label "Labour")]
            [:stretch 2 (ui/label (str (get settlement :labour/price) "g") {:paint (ui.bits/compare->fill settlement :labour/last-demand :labour/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
            [:stretch 1 (ui/label (let [vel (get settlement :labour/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
            [:stretch 1 nil])
          (ui/gap 0 4)
          (ui/row
            [:stretch 4 nil]
            [:stretch 2 (ui/label (str (get settlement :food/market-price) "g"))]
            [:stretch 2 nil]
            [:stretch 4 nil]
            [:stretch 2 (ui/label (str (get settlement :clothes/market-price) "g"))]
            [:stretch 2 nil]
            [:stretch 4 nil]
            [:stretch 2 (ui/label (str (get settlement :labour/market-price) "g"))]
            [:stretch 2 nil])
          (ui/gap 0 2)
          #_
          (interpose (ui/gap 0 4)
            (for [columns [["" "Food" "Clothes" "Labour"] ["Price" :food/price :clothes/price :labour/price] ["Float" :food/price-float :clothes/price-float :labour/price-float] ["Velocity" :food/price-velocity :clothes/price-velocity :labour/price-velocity] ["Demand" :food/last-demand :clothes/last-demand :labour/last-demand] ["Supply" :food/last-supply :clothes/last-supply :labour/last-supply]]]
              (ui/row
                (interpose (ui/gap 4 0)
                  (for [k columns
                        :let [label (if (string? k) k (str (get settlement k)))]]
                    (ui/padding 0 0 40 0
                      (ui/label label {:font font-small :paint fill-black})))))))
          (ui/padding 10
            (ui/column
              (interpose (ui/gap 4 0)
                (for [peep (mapv db/touch (data/lookup-avet world-db :hometown (:db/id settlement)))]
                  (ui/label (str (select-keys peep [:money :health :food :clothes :inventory :last-sold :planning :kind])) {:font font-small :paint fill-black}))))))))))


(def ui-view
  (ui/dynamic ctx [{:keys [font-small fill-white fill-green fill-yellow fill-light-gray fill-dark-gray]} ctx
                   {:keys [world-db map-view]} @state/*world]
    (ui/column
      (ui/row
        ui.parts/map-ui-view
        (ui/padding 10
          (ui/height 200
            (ui/column
              (ui/dynamic ctx [settlements (basic/settlements-q world-db nil)]
                (if-not (seq settlements)
                  ui.parts/chart-view
                  (ui/column
                    (ui/fill fill-light-gray
                      (ui/padding 4
                        (ui/label "Settlement Info")))
                    (ui/vscrollbar
                      (ui/vscroll
                        (ui/column
                          (interpose (ui/gap 4 0)
                            (for [settlement settlements]
                              (ui/padding 4
                                (city-view settlement)))))))
                    (ui/gap 0 10)
                    ui.parts/chart-view)))))))
      (ui/row
        (ui/clickable
          #(swap! state/*world assoc :map-view :default-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :default-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🗺️" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :temp-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :temp-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌡" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :elev-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :elev-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "📏" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :climate-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :climate-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🌍" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :forage-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :forage-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "🚜" {:font font-small :paint fill-white}))))))
        (ui/clickable
          #(swap! state/*world assoc :map-view :mine-view)
          (ui/hoverable
            (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
              (ui/fill (cond hovered? fill-yellow (= map-view :mine-view) fill-green :else fill-dark-gray)
                (ui/padding 10 10
                  (ui/label "⛏️" {:font font-small :paint fill-white}))))))))))

(def basic-ui-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow
                           green-colour yellow-colour dark-gray-colour]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   world @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            (ui/padding 20
              ui-view)
            (ui/dynamic ctx [{:keys [dbs world-db selected-city selected-market]} ctx]
              (let [db world-db
                    cities (data/lookup-avet db :kind :city)]
                (if-not (seq cities)
                  (ui/gap 0 0)
                  (ui/column
                    (ui/row
                      (interpose (ui/fill fill-dark-gray
                                   (ui/gap 0 0))
                        (for [city cities
                              :let [eid (:db/id city)]]
                          (ui/with-context
                            {:hui/active? (= selected-city eid)}
                            (ui/button
                              #(swap! state/*world assoc :selected-city eid)
                              {:bg-active green-colour
                               :bg-hovered yellow-colour
                               :bg dark-gray-colour
                               :p 10 :border-radius 0}
                              (ui/label (:settlement/name city) {:font font-small :paint fill-white}))))))
                    (ui/row
                      (interpose (ui/fill fill-dark-gray
                                   (ui/gap 0 0))
                        (for [market [:food :clothes :labour]]
                          (ui/column
                            (ui/with-context
                              {:hui/active? (= selected-market market)}
                              (ui/button
                                #(swap! state/*world assoc :selected-market market)
                                {:bg-active green-colour
                                 :bg-hovered yellow-colour
                                 :bg dark-gray-colour
                                 :p 10 :border-radius 0}
                                (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))))
                    (when (and selected-city selected-market)
                      (ui/with-context
                        {:cities cities}
                        market-production-view))))))))))))

(def messages-ui-view
  (ui/on-key-down on-key-pressed-impl
    (ui/padding padding padding
      (ui/dynamic ctx [{:keys [scale face-ui]} ctx]
        (ui/column
          ui.parts/top-bar-ui
          [:stretch 1
           (ui/vscrollbar
             (ui/vscroll
               (ui/column
                 (ui.parts/message-log-ui))))])))))


(def supply-demand-vec (atom nil))

(reset! supply-demand-vec
  (map vector
    (into [] cat [(repeat 50 0) (repeat 10 1)])
    (into [] cat [(repeat 50 1) (repeat 10 0)])))

(defn round-to [n to-nearest]
  (* (Math/round (/ n to-nearest)) to-nearest))

(def debug-ui-view
  (ui/dynamic ctx [supply-demand-vec @supply-demand-vec]
    (let [[velocities _velocity prices _price]
          (->> supply-demand-vec
            (reduce
              (fn [[v velocity p price] [supply demand]]
                (let [_ (println :supply supply :demand demand :velocity velocity :gen-prices (screen-ui/gen-prices supply demand velocity))
                      velocity' (round-to (screen-ui/gen-prices supply demand velocity) 0.001)
                      price' (round-to (+ price velocity') 0.001)]
                  [(conj v velocity') velocity'
                   (conj p price') price']))
              [[0] 0 [1] 1]))]
      (ui/column
        (ui/gap 0 10)
        (ui/label "Velocity")
        (ui/gap 0 5)
        (screen-ui/display-chart 500 100 velocities)
        (ui/label "Prices")
        (ui/gap 0 5)
        (screen-ui/display-chart 500 100 prices)))))

(def city-ui-view
  (ui/dynamic ctx [{:keys [fill-light-gray]} ctx
                   {:keys [world-db] :as world} @state/*world
                   settlements (basic/settlements-q world-db nil)]
    (if-not (seq settlements)
      (ui/gap 0 0)
      (ui/column
        (ui/fill fill-light-gray
          (ui/padding 4
            (ui/label "Settlement Info")))
        (ui/vscrollbar
          (ui/vscroll
            (ui/column
              (interpose (ui/gap 0 4)
                (for [settlement settlements]
                  (ui/fill (paint/fill 0xFFFCFEC8)
                    (ui/padding 4
                      (ui/dynamic ctx [{:keys [font-small fill-black fill-red fill-green fill-light-gray]} ctx
                                       {:keys [map-view]} world]
                        (ui/column
                          (ui/row
                            (interpose (ui/gap 4 0)
                              (for [k [:settlement/name :settlement/place]]
                                (ui/label (str (get settlement k)) {:font font-small :paint fill-black}))))
                          ;;(ui/label (str settlement #_(d/touch settlement)))
                          ;;#_#_#_#_#_#_
                          (ui/gap 0 4)
                          (ui/label (str "Money " (:money (first (get settlement :_governs)))))
                          (ui/gap 0 6)
                          (ui/row
                            [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Food"))) (ui/label "🍲"))]
                            [:stretch 6 (ui/halign 1 1 (ui/label (str (get settlement :food/price) "np") {:paint (ui.bits/compare->fill settlement :food/last-demand :food/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})}))]
                            [:stretch 1 (ui/label (let [vel (get settlement :food/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                            [:stretch 2 nil]
                            [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Clothes"))) (ui/label "👚"))]
                            [:stretch 6 (ui/halign 1 1 (ui/label (str (get settlement :clothes/price) "np") {:paint (ui.bits/compare->fill settlement :clothes/last-demand :clothes/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})}))]
                            [:stretch 1 (ui/label (let [vel (get settlement :clothes/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                            [:stretch 2 nil]
                            [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Labour"))) (ui/label "👷"))]
                            [:stretch 6 (ui/halign 1 1 (ui/label (str (get settlement :labour/price) "np") {:paint (ui.bits/compare->fill settlement :labour/last-demand :labour/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})}))]
                            [:stretch 1 (ui/label (let [vel (get settlement :labour/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                            [:stretch 2 nil])
                          (ui/gap 0 6)
                          (interpose (ui/gap 0 4)
                            (for [peep-row (partition-all 3 (data/lookup-avet world-db :hometown (:db/id settlement)))]
                              (ui/row
                                (interpose (ui/gap 2 0)
                                  (for [peep peep-row]
                                    (ui/tooltip {:anchor :top-right :shackle :top-left}
                                      (ui/fill fill-light-gray (ui/padding 5 (ui/label (str (select-keys peep [:money :health :food :clothes :inventory :last-sold :planning :kind])))))
                                      (ui/fill fill-green (ui/padding 5 (ui/label (str (show-peep peep)))))))))))))))))
              #_(interpose (ui/gap 4 0)
                  (for [settlement (repeat 50 (first settlements))]
                    (ui/padding 4
                      (ui/dynamic ctx [{:keys [font-small fill-black fill-red fill-green fill-light-gray]} ctx
                                       {:keys [world-db map-view]} @state/*world]
                        (ui/column
                          (ui/row
                            (interpose (ui/gap 4 0)
                              (for [k [:settlement/name :settlement/place]]
                                (ui/label (str (get settlement k)) {:font font-small :paint fill-black}))))
                          (ui/gap 0 4)
                          (ui/row
                            (ui/column
                              (ui/label (str "Money " (:money (first (get settlement :_governs)))))
                              (ui/gap 0 2)
                              (ui/row
                                [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Food"))) (ui/label "🍔"))]
                                [:stretch 6 (ui/label (str (get settlement :food/price) "np") {:paint (ui.bits/compare->fill settlement :food/last-demand :food/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
                                [:stretch 1 (ui/label (let [vel (get settlement :food/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                                [:stretch 1 nil]
                                [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Clothes"))) (ui/label "👚"))]
                                [:stretch 6 (ui/label (str (get settlement :clothes/price) "np") {:paint (ui.bits/compare->fill settlement :clothes/last-demand :clothes/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
                                [:stretch 1 (ui/label (let [vel (get settlement :clothes/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                                [:stretch 1 nil]
                                [:stretch 2 (ui/tooltip (ui/fill fill-light-gray (ui/padding 5 (ui/label "Labour"))) (ui/label "👷"))]
                                [:stretch 6 (ui/label (str (get settlement :labour/price) "np") {:paint (ui.bits/compare->fill settlement :labour/last-demand :labour/last-supply {:<-fill fill-red :>-fill fill-green :=-fill fill-black})})]
                                [:stretch 1 (ui/label (let [vel (get settlement :labour/price-velocity)] (cond (< vel 0) "📉" (> vel 0) "📈" (zero? vel) "―")))]
                                [:stretch 1 nil])))))
                      #_(city-view settlement)))))))))))

(defn bounded-view [& children]
  (ui/dynamic _ [size (count children)]
    (ui/with-bounds ::bounds
      (ui/dynamic ctx [height (:height (::bounds ctx))]
        (ui/column
          (for [child children]
            (ui/height (/ height size)
              child)))))))

(def bounded-center-view
  (ui/dynamic ctx [{:keys [font-ui leading fill-text]} ctx]
    (ui/with-bounds ::bounds
      (ui/dynamic ctx [{:keys [face-ui font-ui scale]} ctx
                       height (:height (::bounds ctx))]
        (ui/column
          (ui/height (/ height 2)
            (ui/fill (paint/fill 0xFFCFE8FC)
              (ui/vscrollbar
                (ui/vscroll
                  (apply ui/column
                    (mapv
                      #(let [label (ui/padding 20 leading
                                     (ui/label %))]
                         (ui/hoverable
                           (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                             (if hovered?
                               (ui/fill (paint/fill 0xFFCFE8FC) label)
                               label))))
                      (range 0 100)))))))
          (ui/height (/ height 2)
            (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST"))))))))


(def main-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [font-offset-x font-offset-y emoji-offset-x emoji-offset-y fill-white fill-black map-font emoji-font tick]} ctx
                       {:keys [world-db map-view]} @state/*world]
        (let [terrain-tint (condp = map-view
                             :temp-view (fn [tile] (colour (colour-noise (get tile :temp)) 0 0))
                             :elev-view (fn [tile] (colour 0 (colour-noise (get tile :elev)) 0))
                             :climate-view (fn [tile] (colour (colour-noise (get tile :temp)) (colour-noise (get tile :elev)) 0))
                             :forage-view (fn [tile] (colour 0 (* (get tile :food 0) 40) 0))
                             :mine-view (fn [tile] (colour 0 (* (get tile :rock 0) 40) 0))
                             (fn [tile] (land/render-tile-colour (get tile :biome))))
              unit-data (fn [x y]
                          (let [;; pixel-x and pixel-y
                                coord-x x
                                coord-y y
                                coord [coord-x coord-y]

                                tile (basic/coord-q world-db coord)

                                things (when tile (basic/units-q world-db coord))
                                size (count things)
                                {:keys [glyph] :as thing} (when-not (zero? size) (nth things (rem tick size)))

                                {name :settlement/name :as settlement} (when tile (first (basic/settlements-q world-db coord)))]
                            (cond
                              thing [glyph (colour (min 255 (* (get thing :wealth 0) 25)) 0 0) emoji-font emoji-offset-x emoji-offset-y]
                              (seq settlement) [name (colour 0 0 155) map-font font-offset-x font-offset-y]
                              :else ["" (terrain-tint tile) map-font font-offset-x font-offset-y])))]
          (ui/fill (paint/fill 0xFFCFE8FC)
            #_ui.parts/map-ui-view
            (ui/dynamic ctx [{:keys [lrtb cell]} ctx
                             {:keys [camera tick zoom]} @state/*state]
              (ui/with-bounds ::bounds
                (ui/dynamic ctx [{:keys [width height]} (::bounds ctx)]
                  (ui/valign 0.5
                    (ui/halign 0.5
                      (let [cell-width (quot width cell) cell-height (quot height cell)
                            half-x (quot cell-width 2) half-y (quot cell-height 2)
                            [left right top bottom] [(- (first camera) half-x) (+ (first camera) half-x) (- (second camera) half-y) (+ (second camera) half-y)]]
                        (ui/column
                          (for [y-idx (range top bottom)]
                            (ui/row
                              (for [x-idx (range left right)]
                                (ui/hoverable
                                  (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                                    #_(ui/dynamic _ [x-idx x-idx
                                                     y-idx y-idx])
                                    (let [[glyph tile-colour font] (unit-data x-idx y-idx)]
                                      #_(ui/hoverable
                                          (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]))
                                      (ui/fill #_(paint/fill tile-colour)
                                        (if hovered?
                                          (doto (Paint.) (.setColor (unchecked-int 0xFFE1EFFA)))
                                          (paint/fill tile-colour))
                                        (ui/width cell
                                          (ui/halign 0.5
                                            (ui/height cell
                                              (ui/valign 0.5
                                                (ui/label glyph {:font font :paint fill-white}))))))))))))))))))))))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])

(def top-ui-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            main-center-area-ui
            #_
            [:stretch 1 #_bounded-center-view
             (bounded-view
               (ui/fill (paint/fill 0xFFCFE8FC)
                 (ui/vscrollbar
                   (ui/vscroll
                     (apply ui/column
                       (mapv
                         #(let [label (ui/padding 20 2
                                        (ui/label %))]
                            (ui/hoverable
                              (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                                (if hovered?
                                  (ui/fill (paint/fill 0xFFCFE8FC) label)
                                  label))))
                         (range 0 100))))))
               (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST"))
               (ui/fill (paint/fill 0xFFCCFFE8) (ui/label "TEST")))]
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :default-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :default-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "🗺️" {:font font-small :paint fill-white}))))))
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :temp-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :temp-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "🌡" {:font font-small :paint fill-white}))))))
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :elev-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :elev-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "📏" {:font font-small :paint fill-white}))))))
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :climate-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :climate-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "🌍" {:font font-small :paint fill-white}))))))
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :forage-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :forage-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "🚜" {:font font-small :paint fill-white}))))))
                     (ui/clickable
                       #(swap! state/*world assoc :map-view :mine-view)
                       (ui/hoverable
                         (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                           (ui/fill (cond hovered? fill-yellow (= map-view :mine-view) fill-green :else fill-dark-gray)
                             (ui/padding 10 10
                               (ui/label "⛏️" {:font font-small :paint fill-white})))))))
                   #_(ui/row
                       (ui/padding 10 10
                         (ui/label "" {:font font-small :paint fill-black})))))])
            #_[:stretch 1
               (ui/padding 5
                 (ui/fill (paint/fill 0xFFE1EFFA)
                   (ui/column
                     [:stretch 1
                      (ui/row
                        [:stretch 1
                         (ui/row
                           ui.parts/map-ui-view
                           (ui/gap 4 0)
                           city-ui-view
                           #_(ui/dynamic ctx [settlements (basic/settlements-q world-db nil)]
                               (if-not (seq settlements)
                                 ui.parts/chart-view
                                 (ui/column
                                   (ui/fill fill-light-gray
                                     (ui/padding 4
                                       (ui/label "Settlement Info")))
                                   (ui/vscrollbar
                                     (ui/vscroll
                                       (ui/column
                                         (interpose (ui/gap 4 0)
                                           (for [settlement settlements]
                                             (ui/padding 4
                                               (city-view settlement)))))))
                                   (ui/gap 0 10)
                                   ui.parts/chart-view))))])])))]
            #_(ui/valign 0.5
                (ui/row
                  [:stretch 1 nil]
                  [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                                (ui/column
                                  [:stretch 1 (ui/halign 1 0 (ui/label "Right to left (1 0)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0.5 0 (ui/label "Center to left (0.5 0)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0.6 0.2 (ui/label "Arbitrary (0.6 0.2)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0 (ui/label "Left to left (0 0)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 1 0.5 (ui/label "Right to center (1 0.5)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0.5 (ui/label "Center to center (0.5 0.5)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0 0.5 (ui/label "Left to center (0 0.5)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 1 1 (ui/label "Right to right (1 1)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0.5 1 (ui/label "Center to right (0.5 1)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/halign 0 1 (ui/label "Left to right (0 1)"))]
                                  (ui/gap 0 1)
                                  [:stretch 1 (ui/label "Stretch")]))]
                  [:stretch 1 nil]
                  [:stretch 2 (ui/fill (paint/fill 0xFFE1EFFA)
                                (ui/row
                                  [:stretch 1 (ui/valign 1 0 (ui/label "Bottom to top"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0.5 0 (ui/label "Middle to top"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0 (ui/label "Top to top"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 1 0.5 (ui/label "Bottom to middle"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0.5 (ui/label "Middle to middle"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0 0.5 (ui/label "Top to middle"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 1 1 (ui/label "Bottom to bottom"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0.5 1 (ui/label "Middle to bottom"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/valign 0 1 (ui/label "Top to bottom"))]
                                  (ui/gap 1 0)
                                  [:stretch 1 (ui/label "Stretch")]))]
                  [:stretch 1 nil]))))))))

(def economy-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick]} ctx
                       {:keys [world-db selected-city selected-market]} @state/*world]
        (let [cities (data/lookup-avet world-db :kind :city)]
          (if-not (seq cities)
            (ui/gap 0 0)
            (ui/fill fill-light-gray
              (ui/column
                (ui/row
                  (for [city cities
                        :let [eid (:db/id city)]]
                    (ui/with-context
                      {:hui/active? (= selected-city eid)}
                      (ui/button
                        #(swap! state/*world assoc :selected-city eid)
                        {:bg-active green-colour
                         :bg-hovered yellow-colour
                         :bg dark-gray-colour
                         :p 10 :border-radius 0}
                        (ui/label (:settlement/name city) {:font font-small :paint fill-white})))))
                (ui/row
                  (for [market [:food :clothes :labour]]
                    (ui/with-context
                      {:hui/active? (= selected-market market)}
                      (ui/button
                        #(swap! state/*world assoc :selected-market market)
                        {:bg-active green-colour
                         :bg-hovered yellow-colour
                         :bg dark-gray-colour
                         :p 10 :border-radius 0}
                        (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))
                [:stretch 1
                 (ui/row
                   [:stretch 1
                    (ui/fill fill-white
                      (when (and selected-city selected-market)
                        (let [city (db/entity world-db selected-city)
                              producers (into [] (filter #(contains? (set (market-keys-fn selected-market :producers)) (:kind %))) (:_hometown city))
                              demanders (into [] (filter #(contains? (set (market-keys-fn selected-market :demanders)) (:kind %))) (:_hometown city))

                              label (market-label-fn selected-market)
                              price ((market-keys-fn selected-market :price) city)
                              price-history ((market-keys-fn selected-market :price-history) city)
                              produced ((market-keys-fn selected-market :last-produced) city)
                              consumed ((market-keys-fn selected-market :last-consumed) city)
                              total-production (reduce (fn [v m] (+ v ((market-keys-fn selected-market :last-produced) m))) 0 cities)
                              percent-of-production (if (zero? total-production) 0 (* (/ produced total-production) 100))

                              price-belief (market-keys-fn selected-market :price-belief)]
                          (ui/column
                            (ui/row
                              (ui/padding 20
                                (ui/column
                                  (ui/label label)
                                  (ui/gap 0 5)
                                  (ui/label (clojure.pprint/cl-format nil "~,2f% of world production" percent-of-production)))))
                            (ui/row
                              (ui/with-context
                                {:price price :price-history price-history}
                                screen-ui/price-chart-ui))
                            (ui/row
                              [:stretch 1
                               (ui/padding 20
                                 (ui/column
                                   (ui/label "Produced by:")
                                   (ui/gap 0 4)
                                   (ui/row
                                     [:stretch 1
                                      (ui/vscrollbar
                                        (ui/vscroll
                                          (ui/column
                                            (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                              (for [producer producers]
                                                (ui/fill fill-light-gray
                                                  (ui/halign 0.5
                                                    (ui/padding 150 20 150 20
                                                      (ui/label (str ((:kind producer) kind->emoji) " " (pr-str (select-keys producer [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced #_:food/last-consumed #_:clothes/last-consumed #_:labour/last-consumed price-belief]))))))))))))])
                                   (ui/padding 20
                                     (ui/label (str "Total Produced: " produced)))))]
                              [:stretch 1
                               (ui/padding 20
                                 (ui/column
                                   (ui/label "Used by:")
                                   (ui/gap 0 4)
                                   (ui/row
                                     [:stretch 1
                                      (ui/vscrollbar
                                        (ui/vscroll
                                          (ui/column
                                            (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                              (for [demander demanders]
                                                (ui/fill fill-light-gray
                                                  (ui/halign 0.5
                                                    (ui/padding 150 20 150 20
                                                      (ui/label (str ((:kind demander) kind->emoji) " " (pr-str (select-keys demander [:kind :inventory #_:last-sold #_:food/last-produced #_:clothes/last-produced #_:labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed price-belief]))))))))))))])
                                   (ui/padding 20
                                     (ui/label (str "Total Used: " consumed)))))])))))])])))))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])


(def economy-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            economy-center-area-ui
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(def ui-test-view
  (ui/dynamic ctx [{:keys [font-ui leading fill-text]} ctx]
    (ui/row
      (ui/column
        (ui/fill (paint/fill 0xFFCFE8FC)
          (ui/vscrollbar
            (ui/vscroll
              (apply ui/column
                (mapv
                  #(let [label (ui/padding 20 leading
                                 (ui/label %))]
                     (ui/hoverable
                       (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                         (if hovered?
                           (ui/fill (paint/fill 0xFFCFE8FC) label)
                           label))))
                  (range 0 100))))))
        (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST")))
      (ui/gap 4 0)
      (ui/column
        [:stretch 1 (ui/fill (paint/fill 0xFFCFE8FC)
                      (ui/vscrollbar
                        (ui/vscroll
                          (apply ui/column
                            (mapv
                              #(let [label (ui/padding 20 leading
                                             (ui/label %))]
                                 (ui/hoverable
                                   (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                                     (if hovered?
                                       (ui/fill (paint/fill 0xFFCFE8FC) label)
                                       label))))
                              (range 0 100))))))]
        [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST"))])
      (ui/gap 4 0)
      (ui/column
        [:stretch 1 (ui/fill (paint/fill 0xFFCFE8FC)
                      (ui/vscrollbar
                        (ui/vscroll
                          (apply ui/column
                            (mapv
                              #(let [label (ui/padding 20 leading
                                             (ui/label %))]
                                 (ui/hoverable
                                   (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                                     (if hovered?
                                       (ui/fill (paint/fill 0xFFCFE8FC) label)
                                       label))))
                              (range 0 100))))))]
        [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST"))]
        [:stretch 1 (ui/fill (paint/fill 0xFFE8FCCF) (ui/label "TEST"))])
      (ui/gap 4 0)
      (bounded-view
        (ui/fill (paint/fill 0xFFCFE8FC)
          (ui/vscrollbar
            (ui/vscroll
              (apply ui/column
                (mapv
                  #(let [label (ui/padding 20 leading
                                 (ui/label %))]
                     (ui/hoverable
                       (ui/dynamic ctx [hovered? (:hui/hovered? ctx)]
                         (if hovered?
                           (ui/fill (paint/fill 0xFFCFE8FC) label)
                           label))))
                  (range 0 100))))))
        (ui/fill (paint/fill 0xFFFCCFE8) (ui/label "TEST"))
        (ui/fill (paint/fill 0xFFE8FCCF) (ui/label "TEST")))

      (ui/gap 4 0)
      [:stretch 1
       (ui/column
         [:stretch 1 (ui/fill (paint/fill 0xFFCFE8FC)
                       (ui/label "TEST"))])])))

(def economy-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick]} ctx
                       {:keys [world-db selected-city selected-market]} @state/*world]
        (let [cities (data/lookup-avet world-db :kind :city)]
          (if-not (seq cities)
            (ui/gap 0 0)
            (ui/fill fill-light-gray
              (ui/column
                (ui/row
                  (for [city cities
                        :let [eid (:db/id city)]]
                    (ui/with-context
                      {:hui/active? (= selected-city eid)}
                      (ui/button
                        #(swap! state/*world assoc :selected-city eid)
                        {:bg-active green-colour
                         :bg-hovered yellow-colour
                         :bg dark-gray-colour
                         :p 10 :border-radius 0}
                        (ui/label (:settlement/name city) {:font font-small :paint fill-white})))))
                (ui/row
                  (for [market [:food :clothes :labour]]
                    (ui/with-context
                      {:hui/active? (= selected-market market)}
                      (ui/button
                        #(swap! state/*world assoc :selected-market market)
                        {:bg-active green-colour
                         :bg-hovered yellow-colour
                         :bg dark-gray-colour
                         :p 10 :border-radius 0}
                        (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))
                [:stretch 1
                 (ui/row
                   [:stretch 1
                    (ui/fill fill-white
                      (when (and selected-city selected-market)
                        (let [city (db/entity world-db selected-city)
                              producers (into [] (filter #(contains? (set (market-keys-fn selected-market :producers)) (:kind %))) (:_hometown city))
                              demanders (into [] (filter #(contains? (set (market-keys-fn selected-market :demanders)) (:kind %))) (:_hometown city))

                              label (market-label-fn selected-market)
                              price ((market-keys-fn selected-market :price) city)
                              price-history ((market-keys-fn selected-market :price-history) city)
                              produced ((market-keys-fn selected-market :last-produced) city)
                              consumed ((market-keys-fn selected-market :last-consumed) city)
                              total-production (reduce (fn [v m] (+ v ((market-keys-fn selected-market :last-produced) m))) 0 cities)
                              percent-of-production (if (zero? total-production) 0 (* (/ produced total-production) 100))

                              price-belief (market-keys-fn selected-market :price-belief)]
                          (ui/column
                            (ui/row
                              (ui/padding 20
                                (ui/column
                                  (ui/label label)
                                  (ui/gap 0 5)
                                  (ui/label (clojure.pprint/cl-format nil "~,2f% of world production" percent-of-production)))))
                            (ui/row
                              (ui/with-context
                                {:price price :price-history price-history}
                                screen-ui/price-chart-ui))
                            (ui/row
                              [:stretch 1
                               (ui/padding 20
                                 (ui/column
                                   (ui/label "Produced by:")
                                   (ui/gap 0 4)
                                   (ui/row
                                     [:stretch 1
                                      (ui/vscrollbar
                                        (ui/vscroll
                                          (ui/column
                                            (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                              (for [producer producers]
                                                (ui/fill fill-light-gray
                                                  (ui/halign 0.5
                                                    (ui/padding 150 20 150 20
                                                      (ui/label (str ((:kind producer) kind->emoji) " " (pr-str (select-keys producer [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced #_:food/last-consumed #_:clothes/last-consumed #_:labour/last-consumed price-belief]))))))))))))])
                                   (ui/padding 20
                                     (ui/label (str "Total Produced: " produced)))))]
                              [:stretch 1
                               (ui/padding 20
                                 (ui/column
                                   (ui/label "Used by:")
                                   (ui/gap 0 4)
                                   (ui/row
                                     [:stretch 1
                                      (ui/vscrollbar
                                        (ui/vscroll
                                          (ui/column
                                            (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                              (for [demander demanders]
                                                (ui/fill fill-light-gray
                                                  (ui/halign 0.5
                                                    (ui/padding 150 20 150 20
                                                      (ui/label (str ((:kind demander) kind->emoji) " " (pr-str (select-keys demander [:kind :inventory #_:last-sold #_:food/last-produced #_:clothes/last-produced #_:labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed price-belief]))))))))))))])
                                   (ui/padding 20
                                     (ui/label (str "Total Used: " consumed)))))])))))])]))))



        #_(let [db world-db
                cities (data/lookup-avet db :kind :city)]
            (if-not (seq cities)
              (ui/gap 0 0)
              (ui/column
                (ui/row
                  (interpose (ui/fill fill-dark-gray
                               (ui/gap 0 0))
                    (for [city cities
                          :let [eid (:db/id city)]]
                      (ui/with-context
                        {:hui/active? (= selected-city eid)}
                        (ui/button
                          #(swap! state/*world assoc :selected-city eid)
                          {:bg-active green-colour
                           :bg-hovered yellow-colour
                           :bg dark-gray-colour
                           :p 10 :border-radius 0}
                          (ui/label (:settlement/name city) {:font font-small :paint fill-white}))))))
                (ui/row
                  (interpose (ui/fill fill-dark-gray
                               (ui/gap 0 0))
                    (for [market [:food :clothes :labour]]
                      (ui/column
                        (ui/with-context
                          {:hui/active? (= selected-market market)}
                          (ui/button
                            #(swap! state/*world assoc :selected-market market)
                            {:bg-active green-colour
                             :bg-hovered yellow-colour
                             :bg dark-gray-colour
                             :p 10 :border-radius 0}
                            (ui/label (market-label-fn market) {:font font-small :paint fill-white})))))))
                (when (and selected-city selected-market)
                  (ui/with-context
                    {:cities cities}
                    #_market-production-view
                    (let [city (db/entity world-db selected-city)

                          producers (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :producers))
                          demanders (into [] (comp (map (lookup-by-kind world-db)) cat (map db/touch)) (market-keys-fn selected-market :demanders))

                          label (market-label-fn selected-market)
                          price ((market-keys-fn selected-market :price) city)
                          price-history ((market-keys-fn selected-market :price-history) city)
                          produced ((market-keys-fn selected-market :last-produced) city)
                          consumed ((market-keys-fn selected-market :last-consumed) city)
                          total-production (reduce (fn [v m] (+ v ((market-keys-fn selected-market :last-produced) m))) 0 cities)
                          percent-of-production (if (zero? total-production) 0 (* (/ produced total-production) 100))]
                      (ui/column
                        (ui/row
                          (ui/padding 20
                            (ui/column
                              (ui/label label)
                              (ui/gap 0 5)
                              (ui/label (clojure.pprint/cl-format nil "~,2f% of world production" percent-of-production))))
                          (ui/with-context
                            {:price price :price-history price-history}
                            screen-ui/price-chart-ui))
                        (ui/row
                          (ui/padding 20
                            (ui/column
                              (ui/label "Produced by:")
                              (ui/gap 0 4)
                              (ui/row
                                (ui/vscrollbar
                                  (ui/vscroll
                                    (ui/column
                                      (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                        (for [producer producers]
                                          (ui/tooltip {:anchor :top-right :shackle :top-right}
                                            (ui/label (pr-str producer))
                                            (ui/fill fill-light-gray
                                              (ui/padding 150 20 150 20
                                                (ui/label (str ((:kind producer) kind->emoji) " " (pr-str (select-keys producer [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed])))))))))))))
                              (ui/padding 20
                                (ui/label (str "Total Produced: " produced)))))
                          (ui/padding 20
                            (ui/column
                              (ui/label "Used by:")
                              (ui/gap 0 4)
                              (ui/row
                                (ui/vscrollbar
                                  (ui/vscroll
                                    (ui/column
                                      (interpose (ui/fill fill-dark-gray (ui/gap 0 4))
                                        (for [demander demanders]
                                          (ui/tooltip {:anchor :top-right :shackle :top-right}
                                            (ui/label (pr-str demander))
                                            (ui/fill fill-light-gray
                                              (ui/padding 150 20 150 20
                                                (ui/label (str ((:kind demander) kind->emoji) " " (pr-str (select-keys demander [:kind :inventory :last-sold :food/last-produced :clothes/last-produced :labour/last-produced :food/last-consumed :clothes/last-consumed :labour/last-consumed])))))))))))))
                              (ui/padding 20
                                (ui/label (str "Total Used: " consumed))))))

                        ;;#_#_#_
                        (ui/label "Cities:")
                        (ui/gap 0 2)
                        (ui/height 100
                          (ui/row
                            (ui/vscrollbar
                              (ui/vscroll
                                (ui/column
                                  (interpose (ui/fill fill-dark-gray
                                               (ui/gap 0 4))
                                    (for [{:keys [settlement/name] :as market} cities]
                                      (ui/column
                                        (ui/label name)
                                        (ui/gap 0 2)
                                        ;;(ui/label (pr-str (db/touch market)))
                                        ;;(ui/gap 0 2)
                                        (show-map-ui market font-small fill-black)))))))))))
                    ,))))))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])

(def economy-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            economy-center-area-ui
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(def tax-controls-ui
  (ui/dynamic ctx [{:keys [player-eid]} ctx]
    (let [min-rate 0
          max-rate 50
          inc-tax-by (fn [tax-rate n] (min (+ tax-rate n) max-rate))
          dec-tax-by (fn [tax-rate n] (max (- tax-rate n) min-rate))
          inc-tax-db-fn (fn [db inc-by]
                          (let [{:keys [tax-rate]} (data/entity db player-eid)]
                            [[:db/add player-eid :tax-rate (inc-tax-by tax-rate inc-by)]]))
          dec-tax-db-fn (fn [db dec-by]
                          (let [{:keys [tax-rate]} (data/entity db player-eid)]
                            [[:db/add player-eid :tax-rate (dec-tax-by tax-rate dec-by)]]))]
      (ui/column
        (ui/row
          (ui/padding 10 (ui/label "Tax Rate"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-tax-db-fn 5]])
            {:border-radius 0}
            (ui/label "+5%"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-tax-db-fn 1]])
            {:border-radius 0}
            (ui/label "+1%"))
          (ui/dynamic _ [{:keys [world-conn]} @state/*world
                         world-db @world-conn]
            (let [{:keys [tax-rate]} (data/entity world-db player-eid)]
              (ui/padding 10 (ui/label (str tax-rate "%")))))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-tax-db-fn 1]])
            {:border-radius 0}
            (ui/label "-1%"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-tax-db-fn 5]])
            {:border-radius 0}
            (ui/label "-5%")))))))

(defn stockpile-controls-ui-block [{:keys [stockpile-name stockpile-key will-buy-key will-sell-key stockpile-buy-price-key stockpile-sell-price-key]}]
  (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid player]} ctx]
    (let [toggle-buy-db-fn (fn [db]
                             (let [will-buy? (get (data/entity db player-eid) will-buy-key)]
                               [[:db/add player-eid will-buy-key (not will-buy?)]]))
          toggle-sell-db-fn (fn [db]
                              (let [will-sell? (get (data/entity db player-eid) will-sell-key)]
                                [[:db/add player-eid will-sell-key (not will-sell?)]]))
          min-price 1
          max-price 99999
          inc-price-by (fn [price n] (min ((fnil + 0) price n) max-price))
          dec-price-by (fn [price n] (max ((fnil - 0) price n) min-price))
          inc-price-db-fn (fn [db price-key inc-by]
                            (let [player (data/entity db player-eid)]
                              [[:db/add player-eid price-key (inc-price-by (price-key player) inc-by)]]))
          dec-price-db-fn (fn [db price-key dec-by]
                            (let [player (data/entity db player-eid)]
                              [[:db/add player-eid price-key (dec-price-by (price-key player) dec-by)]]))]
      (ui/column
        (ui/row
          (ui/padding 10 (ui/label (str stockpile-name " Stockpiled")))
          (ui/dynamic _ [{:keys [world-conn]} @state/*world
                         world-db @world-conn]
            (let [food-stockpile (get (data/entity world-db player-eid) stockpile-key 0)]
              (ui/padding 10 (ui/label food-stockpile))))
          (ui/clickable
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call toggle-buy-db-fn]])
            (ui/dynamic _ [{:keys [world-conn]} @state/*world
                           world-db @world-conn]
              (let [will-buy? (get (data/entity world-db player-eid) will-buy-key)]
                (ui/fill (paint/fill (if will-buy? green-colour dark-gray-colour))
                  (ui/padding 20 12 20 12
                    (ui/halign 0.5
                      (ui/label (str "Buy " stockpile-name))))))))
          (ui/clickable
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call toggle-sell-db-fn]])
            (ui/dynamic _ [{:keys [world-conn]} @state/*world
                           world-db @world-conn]
              (let [will-sell? (get (data/entity world-db player-eid) will-sell-key)]
                (ui/fill (paint/fill (if will-sell? green-colour dark-gray-colour))
                  (ui/padding 20 12 20 12
                    (ui/halign 0.5
                      (ui/label (str "Sell " stockpile-name)))))))))
        (ui/row
          (ui/padding 10 (ui/label (str stockpile-name " Buy @ Price")))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-price-db-fn stockpile-buy-price-key 5]])
            {:border-radius 0}
            (ui/label "+5np"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-price-db-fn stockpile-buy-price-key 1]])
            {:border-radius 0}
            (ui/label "+1np"))
          (ui/dynamic _ [{:keys [world-conn]} @state/*world
                         world-db @world-conn]
            (let [stockpile-buy-price (get (data/entity world-db player-eid) stockpile-buy-price-key 1)]
              (ui/padding 10 (ui/label (str stockpile-buy-price "np")))))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-price-db-fn stockpile-buy-price-key 1]])
            {:border-radius 0}
            (ui/label "-1np"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-price-db-fn stockpile-buy-price-key 5]])
            {:border-radius 0}
            (ui/label "-5np")))
        (ui/row
          (ui/padding 10 (ui/label (str stockpile-name " Sell @ Price")))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-price-db-fn stockpile-sell-price-key 5]])
            {:border-radius 0}
            (ui/label "+5np"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call inc-price-db-fn stockpile-sell-price-key 1]])
            {:border-radius 0}
            (ui/label "+1np"))
          (ui/dynamic _ [{:keys [world-conn]} @state/*world
                         world-db @world-conn]
            (let [food-stockpile-sell-price (get (data/entity world-db player-eid) stockpile-sell-price-key 1)]
              (ui/padding 10 (ui/label (str food-stockpile-sell-price "np")))))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-price-db-fn stockpile-sell-price-key 1]])
            {:border-radius 0}
            (ui/label "-1np"))
          (ui/button
            #(swap! state/*world basic/transact-with-conn [[:db.fn/call dec-price-db-fn stockpile-sell-price-key 5]])
            {:border-radius 0}
            (ui/label "-5np")))))))

(def stockpile-controls-ui
  (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid player]} ctx]
    (ui/column
      (stockpile-controls-ui-block {:stockpile-name "Food" :stockpile-key :food-stockpile :will-buy-key :food-stockpile-buy? :will-sell-key :food-stockpile-sell? :stockpile-buy-price-key :food-stockpile-buy-price :stockpile-sell-price-key :food-stockpile-sell-price})
      (ui/gap 0 4)
      (stockpile-controls-ui-block {:stockpile-name "Clothes" :stockpile-key :clothes-stockpile :will-buy-key :clothes-stockpile-buy? :will-sell-key :clothes-stockpile-sell? :stockpile-buy-price-key :clothes-stockpile-buy-price :stockpile-sell-price-key :clothes-stockpile-sell-price}))))

(def infrastructure-controls-ui
  (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid player]} ctx]
    (let []
      (ui/column
        (ui/dynamic _ [{:keys [world-conn]} @state/*world
                       world-db @world-conn]
          (ui/label (str (select-keys (d/entity world-db player-eid) [:infrastructure/maintenance :infrastructure/hp :infrastructure/labour-bought :infrastructure/repair :infrastructure/size]))))))))

(def control-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid]} ctx
                       {:keys [world-db]} @state/*world]
        (if player-eid
          (let [player (data/entity world-db player-eid)]
            (ui/with-context
              {:player player}
              (ui/column
                tax-controls-ui
                (ui/gap 0 4)
                stockpile-controls-ui
                (ui/gap 0 4)
                infrastructure-controls-ui
                ;;building-controls-ui
                (ui/gap 0 4))))
          (ui/padding 10
            (ui/label "Please Select a City to play as!"))))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])

(def control-ui-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            control-center-area-ui
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(defn reduce-to-squares [size]
  (reduce
    (fn [[sz v] p10]
      (let [n (long (Math/pow 10 p10))]
        (if (zero? p10)
          (conj v [n sz])
          [(rem sz n) (conj v [(inc p10) (quot sz n)])])))
    [size []]
    (range (long (Math/log10 size)) -1 -1)))

(def *picked (atom 0))

(def value->fill-colour
  {1 (paint/fill 0xFFCECECE)
   2 (paint/fill 0xFFDC96A2)
   3 (paint/fill 0xFFC1B75B)
   4 (paint/fill 0xFF8CBDC4)
   5 (paint/fill 0xFFAB8219)
   6 (paint/fill 0xFF91728B)})

(def foo-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid]} ctx
                       {:keys [world-db]} @state/*world
                       picked @*picked]
        (if player-eid
          (let [tax-rate (:tax-rate (data/entity world-db player-eid))
                min-rate 0
                max-rate 50
                inc-tax-by (fn [n] (min (+ tax-rate n) max-rate))
                dec-tax-by (fn [n] (max (- tax-rate n) min-rate))]
            (ui/column
              (ui/row
                (ui/padding 10 (ui/label "Tax Rate"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "+5%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "+1%"))
                (ui/padding 10 (ui/label (str tax-rate "%")))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "-1%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "-5%")))))
          #_(ui/gap 0 0)
          (ui/column
            [:stretch 1
             (ui/row
               (ui/button
                 #(swap! *picked inc)
                 {:border-radius 0}
                 (ui/label "+"))
               (ui/padding 10
                 (ui/label (str picked)))
               (ui/button
                 #(swap! *picked (fn [x] (max 0 (dec x))))
                 {:border-radius 0}
                 (ui/label "-")))]
            [:stretch 12
             (ui/row
               (ui/vscrollbar
                 (ui/vscroll
                   (let [trades (data/lookup-avet world-db :kind :trade)]
                     (ui/column
                       (ui/gap 0 6)
                       (interpose (ui/gap 0 4)
                        (for [trade (take 10 (drop (* picked 10) trades))
                              :let [matched (:matched trade)
                                    traded? (seq matched)]]
                          (ui/column
                            (ui/label (str (keys trade)))
                            (ui/gap 0 6)
                            ;; Display as boxes with a tooltip for more detail?
                            (let [{:keys [buys sell market] :as summary} (get-in trade [:summary :level])]
                              (ui/column
                                ;;(ui/label (str (dissoc summary :buys :sell)))
                                (ui/label (str market (when traded? (str " Sold " (:sold trade) " Open " (:open-price trade)  " High " (:high-price trade)  " Low " (:low-price trade) " Close " (:close-price trade) " Price " (:current-price trade)))))
                                (interpose (ui/gap 0 4)
                                  (for [[price orders] buys]
                                    (ui/padding 0 2 0 2
                                      (ui/row
                                        (ui/valign 0.5
                                          (ui/padding 2 0 18 0
                                            (ui/label (str price))))
                                        (interpose (ui/gap 2 4)
                                          (for [{:keys [size] :as order} orders]
                                            (ui/valign 0.5
                                              (ui/tooltip {:anchor :bottom-left :shackle :top-left}
                                                (ui/fill fill-light-gray
                                                  (ui/padding 4
                                                    (ui/label (str order))))
                                                (ui/fill fill-white
                                                  (ui/padding 4
                                                    (ui/row
                                                      (interpose (ui/gap 2 2)
                                                        (for [[size num] (reduce-to-squares size)
                                                              _idx (range num)
                                                              :let [size' (* size 3)]]
                                                          (ui/valign 0.5
                                                            (ui/column
                                                              (ui/fill (get value->fill-colour size fill-black)
                                                                (ui/gap size' size')))))))))))))))))
                                (ui/halign 1
                                  (ui/column
                                    (interpose (ui/gap 0 4)
                                      (for [[price orders] sell]
                                        (ui/padding 0 2 0 2
                                          (ui/row
                                            (interpose (ui/gap 2 4)
                                              (for [{:keys [size] :as order} orders]
                                                (ui/valign 0.5
                                                  (ui/tooltip {:anchor :bottom-left :shackle :top-left}
                                                    (ui/fill fill-light-gray
                                                      (ui/padding 4
                                                        (ui/label (str order))))
                                                    (ui/fill fill-white
                                                      (ui/padding 4
                                                        (ui/row
                                                          (interpose (ui/gap 2 2)
                                                            (for [[size num] (reduce-to-squares size)
                                                                  _idx (range num)
                                                                  :let [size' (* size 3)]]
                                                              (ui/valign 0.5
                                                                (ui/column
                                                                  (ui/fill (get value->fill-colour size fill-black)
                                                                    (ui/gap size' size')))))))))))))
                                            (ui/valign 0.5
                                              (ui/padding 18 0 2 0
                                                (ui/label (str price))))))))))))
                            (if traded?
                              (ui/column
                                (ui/gap 0 6)
                                (ui/label "Trades")
                                (ui/gap 0 4)
                                (interpose (ui/gap 2 4)
                                  (for [{:keys [buy-order sell-order] :as match} matched]
                                    (ui/column
                                      (ui/label (str buy-order))
                                      (ui/label (str sell-order))
                                      (ui/label (str (dissoc match :buy-order :sell-order)))
                                      (ui/label (str ((juxt (comp :good-kw :sell-order) (comp :good-kw :buy-order)) match) " £" (:price match) " " (:size match)))))))
                              (ui/gap 0 0))
                            (ui/gap 0 4)))))
                     #_(ui/column
                         (ui/label "Buyers")
                         (ui/gap 0 4)
                         (interpose (ui/gap 0 4)
                           (for [e (sort-by :buyer trades)
                                 :let [buyer (d/entity world-db (:buyer e))
                                       seller (d/entity world-db (:seller e))]]
                             (ui/label (str [(db/touch e) (db/touch buyer) (db/touch seller)]))))
                         (ui/gap 0 4)
                         (ui/label "Sellers")
                         (ui/gap 0 4)
                         (interpose (ui/gap 0 4)
                           (for [e (sort-by :seller trades)
                                 :let [buyer (d/entity world-db (:buyer e))
                                       seller (d/entity world-db (:seller e))]]
                             (ui/label (str [(db/touch e) (db/touch buyer) (db/touch seller)])))))))))])))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])

(def tax+market-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            foo-center-area-ui
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(def quest-center-area-ui
  [:stretch 1
   (ui/row
     [:stretch 3
      (ui/dynamic ctx [{:keys [fill-white fill-black yellow-colour green-colour fill-light-gray fill-dark-gray dark-gray-colour font-small map-font emoji-font tick player-eid]} ctx
                       {:keys [world-db]} @state/*world]
        (if player-eid
          (let [tax-rate (:tax-rate (data/entity world-db player-eid))
                min-rate 0
                max-rate 50
                inc-tax-by (fn [n] (min (+ tax-rate n) max-rate))
                dec-tax-by (fn [n] (max (- tax-rate n) min-rate))]
            (ui/column
              (ui/row
                ;; Push your luck, sending out an expedition, we want players to ask themselves:
                ;;  - Can I keep going?
                ;;  - Should I take my winnings here?
                ;;  - What does partially walking away look like?
                ;;    - IE reinforcing?
                (ui/padding 10 (ui/label "Tax Rate"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "+5%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "+1%"))
                (ui/padding 10 (ui/label (str tax-rate "%")))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "-1%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "-5%")))
              (ui/row
                (ui/padding 10 (ui/label "# of Wilderness Area"))
                (ui/padding 10 (ui/label "# of Grassland Area"))
                (ui/padding 10 (ui/label "# of Cropland Area")))
              (ui/row
                (ui/padding 10 (ui/label "Barley Rate"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "+5%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (inc-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "+1%"))
                (ui/padding 10 (ui/label (str tax-rate "%")))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 1)]])
                  {:border-radius 0}
                  (ui/label "-1%"))
                (ui/button
                  #(swap! state/*world basic/transact-with-db [[:db/add player-eid :tax-rate (dec-tax-by 5)]])
                  {:border-radius 0}
                  (ui/label "-5%")))))
          (ui/label "Please select a player")))]
     [:stretch 1 (ui/fill (paint/fill 0xFFFCCFE8)
                   city-ui-view)])])

#_
(let [testo-db-uri "asami:mem://testo"
      _ (ad/delete-database testo-db-uri)
      _ (ad/create-database testo-db-uri)
      testo-conn (ad/connect testo-db-uri)
      db (ad/db testo-conn)]
  db)

(def questy-db-uri "asami:multi://questy")

(def quest-center-area-ui-2
  [:stretch 1
   (ui/dynamic _ [conn (ad/connect questy-db-uri)]
     (ui/column
       (ui/row
         (ui/label "Box")
         (ui/label "People"))
       (ui/row
         (ui/button
           #(ad/delete-database questy-db-uri)
           {:border-radius 0}
           (ui/label "Delete"))
         (ui/button
           #(let [db (ad/db conn)
                  g (ad/graph db)
                  n1 (ad/new-node g)
                  n2 (ad/new-node g)
                  n3 (ad/new-node g)
                  n4 (ad/new-node g)]
              (ad/transact conn {:tx-data
                                 #_
                                 [{:food 2 :max-food 4 :coord [0 0] :db/id n1}
                                  ;;{:food 2 :max-food 3 :coord [0 1] :db/id n2}
                                  ;;{:food 2 :max-food 2 :coord [1 0] :db/id n3}
                                  ;;{:food 0 :max-food 1 :coord [1 1] :db/id n4}
                                  {:carrying 0 :vision 1 :hunger 2 :place n1 :db/id n2}
                                  ;;{:carrying 6 :vision 2 :hunger 1 :place n2}
                                  ;;{:carrying 4 :vision 1 :hunger 1 :place n3}
                                  {:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [2 2]}}]
                                 [{:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [0 0]}}
                                  {:carrying 6 :vision 2 :hunger 1 :place {:food 2 :max-food 3 :coord [0 1]}}
                                  {:carrying 4 :vision 1 :hunger 1 :place {:food 2 :max-food 2 :coord [1 0]}}
                                  {:food 0 :max-food 1 :coord [1 1]}]}))
           {:border-radius 0}
           (ui/label "Insert")))
       (ui/row
         (ui/dynamic _ [db (ad/db conn)]
           (ui/column
             (ui/label "Box")
             (for [e (ad/q '[:find [?e ...] :where [?e :coord]] db)]
               (ui/label (str e ": " (ad/entity db e))))
             (for [e (ad/q '[:find [?e ...] :where [?e :carrying]] db)]
               (ui/label (str e ": " (ad/entity db e)))))))))])


(let [conn (ad/connect questy-db-uri)
      db (ad/db conn)]
  (->> (ad/graph db)
    :pos
    :coord
    keys
    (map (partial ad/entity db)))
  #_(ad/transact conn {:update-fn (fn [g tid]
                                    (println :tid tid :g g)
                                    g)})
  #_db
  #_(mapv #_(partial ad/entity db) identity (ad/q '[:find [?p ...] :where [?e :max-food] [?p :place ?p]] db)))
#_
(let [questy-db-uri "asami:mem://testo"
      _ (ad/delete-database questy-db-uri)
      conn (ad/connect questy-db-uri)
      db (ad/db conn)
      g (ad/graph db)
      n1 (ad/new-node g)]
  @(ad/transact conn {:tx-data [{:food 2 :max-food 4 :coord [0 0] :db/id n1}
                                {:carrying 0 :vision 1 :hunger 2 :place n1}]})
  (ad/export-data conn))

(def local* (atom {}))

(let [db (d/db-with (d/empty-db {:day {:db/index true}
                                 :money {:db/index true}
                                 :kind {:db/index true}
                                 :good {:db/index true}
                                 :place {:db/valueType :db.type/ref}
                                 :coord {:db/unique :db.unique/identity}
                                 :settlement/place {:db/valueType :db.type/ref}
                                 :hometown {:db/valueType :db.type/ref}
                                 :governs {:db/valueType :db.type/ref
                                           :db/cardinality :db.cardinality/one}})
           (into [[:db/add -1 :kind :city]
                  [:db/add -1 :settlement/name \A]
                  [:db/add -1 :food/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :clothes/market (fruit-economy.sim.market/empty-order-book)]
                  [:db/add -1 :labour/market (fruit-economy.sim.market/empty-order-book)]
                  {:hometown -1
                   :buildings [{:kind :trader :focus :food :amount 100 :max-amount 100 :buy-price 20 :sell-price 50 :money 1000}
                               {:kind :stockpile :focus :food :amount 100 :max-amount 100}
                               {:kind :empty}
                               {:kind :empty} {:kind :empty} {:kind :empty}
                               {:kind :empty} {:kind :empty} {:kind :empty}]
                   :kind :hold}]
             (drop 8 (:then basic/create-settlement-rule))))
      conn (d/conn-from-db db)]
  (def city* conn))

(comment
  (let [city @city*
        db city #_(d/db-with city
                    [{:buildings [{:kind :trader :focus :food :amount 100 :max-amount 100 :buy-price 20 :sell-price 50}
                                  {:kind :stockpile :focus :food :amount 100 :max-amount 100}
                                  {:kind :empty}
                                  {:kind :empty} {:kind :empty} {:kind :empty}
                                  {:kind :empty} {:kind :empty} {:kind :empty}]
                      :kind :hold}])
        {:keys [buildings hometown]} (d/touch (first (data/lookup-avet db :kind :hold)))
        price (:food/price hometown)
        {:keys [focus buy-price sell-price]} (first buildings)]
    [:buy buy-price :sell sell-price :price price :buy? (<= price buy-price) :sell? (>= price sell-price)]))

(def quest-center-area-ui-3
  [:stretch 1
   (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                            font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                    local @local*
                    city @city*]
     (let [sx (str/split-lines
                (with-out-str
                  (uber/pprint (uber/multidigraph
                                 []))))]
       (ui/column
         (ui/label (str local))
         (ui/gap 0 4)
         (ui/button
           #(swap! local* update-in [:factory :steel] (fnil inc 0))
           (ui/label "Steel Factory +1"))
         (ui/gap 0 4)
         (ui/label "Box:")
         (ui/gap 0 4)
         (interpose (ui/gap 4 4)
           (for [s sx]
             (ui/label s)))
         (ui/gap 0 16)
         (ui/label "Building:")
         (ui/gap 0 4)
         (interpose (ui/gap 0 4)
           (for [hold (data/lookup-avet city :kind :hold)]
             (ui/column
               (ui/label (str (d/touch hold)))
               (for [buildings-col (partition 3 (:buildings hold))]
                 (ui/row
                   (interpose (ui/gap 4 0)
                     (for [building buildings-col]
                       (ui/fill fill-light-gray
                         (ui/padding 25
                           (ui/label (str building)))))))))))
         (ui/gap 0 16)
         (ui/label (str (mapv d/touch (data/lookup-avet city :kind :city))))
         (ui/gap 0 4)
         (ui/row
           (ui/button
             #(swap! city* basic/transact-with-conn [{:inventory [] :kind :building}])
             (ui/label "Steel Factory +1")))
         (ui/gap 0 4)
         (interpose (ui/gap 4 4)
           (for [sx (partition 3 ["Stockpile Food, buy @ 20, 10 stock"
                                  "Stockpile Clothes, buy @ 20, 10 stock"
                                  "Process Food 2 => 1 Clothes"
                                  "Empty Space"
                                  "Empty Space"
                                  "Empty Space"
                                  "Empty Space"
                                  "Empty Space"
                                  "Empty Space"])]
             (ui/row
               (interpose (ui/gap 4 4)
                 (for [s sx]
                   (ui/fill fill-light-gray
                     (ui/padding 25
                       (ui/label s)))))))))))
   #_(ui/dynamic _ [conn (ad/connect questy-db-uri)]
       (ui/column
         (ui/row
           (ui/label "Box")
           (ui/label "People"))
         (ui/row
           (ui/button
             #(ad/delete-database questy-db-uri)
             {:border-radius 0}
             (ui/label "Delete"))
           (ui/button
             #(let [db (ad/db conn)
                    g (ad/graph db)
                    n1 (ad/new-node g)
                    n2 (ad/new-node g)
                    n3 (ad/new-node g)
                    n4 (ad/new-node g)]
                (ad/transact conn {:tx-data
                                   #_
                                   [{:food 2 :max-food 4 :coord [0 0] :db/id n1}
                                    ;;{:food 2 :max-food 3 :coord [0 1] :db/id n2}
                                    ;;{:food 2 :max-food 2 :coord [1 0] :db/id n3}
                                    ;;{:food 0 :max-food 1 :coord [1 1] :db/id n4}
                                    {:carrying 0 :vision 1 :hunger 2 :place n1 :db/id n2}
                                    ;;{:carrying 6 :vision 2 :hunger 1 :place n2}
                                    ;;{:carrying 4 :vision 1 :hunger 1 :place n3}
                                    {:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [2 2]}}]
                                   [{:carrying 0 :vision 1 :hunger 2 :place {:food 2 :max-food 4 :coord [0 0]}}
                                    {:carrying 6 :vision 2 :hunger 1 :place {:food 2 :max-food 3 :coord [0 1]}}
                                    {:carrying 4 :vision 1 :hunger 1 :place {:food 2 :max-food 2 :coord [1 0]}}
                                    {:food 0 :max-food 1 :coord [1 1]}]}))
             {:border-radius 0}
             (ui/label "Insert")))
         (ui/row
           (ui/dynamic _ [db (ad/db conn)]
             (ui/column
               (ui/label "Box")
               (for [e (ad/q '[:find [?e ...] :where [?e :coord]] db)]
                 (ui/label (str e ": " (ad/entity db e))))
               (for [e (ad/q '[:find [?e ...] :where [?e :carrying]] db)]
                 (ui/label (str e ": " (ad/entity db e)))))))))])


(def questing-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            ;;quest-center-area-ui
            ;;quest-center-area-ui-2
            quest-center-area-ui-3
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(def belief-view
  (ui/dynamic ctx [{:keys [scale face-default emoji-face x-scale y-scale
                           font-small fill-white fill-black fill-dark-gray fill-light-gray fill-green fill-yellow]} ctx
                   {:keys [camera tick zoom]} @state/*state
                   {:keys [world-db map-view] :as world} @state/*world
                   econ-graph @ad/econ-graph]
    (let [map-font (Font. ^Typeface face-default (float (* scale 6 zoom)))
          emoji-font (Font. ^Typeface emoji-face (float (* scale 8 zoom)))

          canvas-width (int (* x-scale state/*canvas-width*))
          canvas-height (int (* y-scale state/*canvas-height*))

          {:keys [cell lrtb]} (ui.bits/camera->viewport camera zoom canvas-width canvas-height)]
      (ui/with-context
        (merge
          {:map-font map-font
           :emoji-font emoji-font
           :lrtb lrtb
           :cell cell
           :tick tick}
          world)
        (ui/on-key-down on-key-pressed-impl
          (ui/column
            ui.parts/top-bar-ui
            (let [#_#_#_#_#_#_
                  peep-conn (ad/conn ad/peep-db-uri)
                  db (ad/db peep-conn)
                  graph (ad/graph db)
                  graph (ad/asami->graph econ-graph {:node-label-fn (fn [g n] (str n)) :edge-label-fn (fn [g s d] (str (ffirst (get-in (:osp g) [d s]))))})]
              [:stretch 1
               (ui/row
                 [:stretch 1
                  #_(custom-ui/svg-canvas (ad/view graph))
                  #_(custom-ui/svg-canvas (ad/view @ad/econ-graph))
                  #_(ui/label (str (ad/view graph)))
                  (custom-ui/svg-canvas (ad/graph->svg graph))])])
            (ui/column
              [:stretch 1
               (ui/padding 0 10 0 0
                 (ui/fill fill-light-gray
                   (ui/row
                     (ui/gap 30 30))))])))))))

(comment
  ;; Build model of peep knowledge
  (let [peep-db-uri ad/peep-db-uri
        _ (ad/delete-database peep-db-uri)
        _ (ad/create-database peep-db-uri)
        peep-conn (ad/connect peep-db-uri)
        db (ad/db peep-conn)
        things [#_{:name :butcher
                   :input [{:name :chicken-carcass}]}
                {:input {:name :chicken-carcass :quantity 1}
                 :output {1 {:name :chicken-meat :quantity 2 :likelihood 1}}}]]
    (-> @(ad/transact peep-conn {:tx-data things})
      #_:tx-data
      :db-after
      ((fn [db] (mapv (partial ad/entity db) (d/q '[:find [?m ...] :where [?m :input _]] db)))))))

(def ui-views
  ;; exploiting the fact that as long as array-map doesn't grow, it keeps insertion order
  (array-map
    "Belief" belief-view
    "Tax + Market" tax+market-view
    "Questing" questing-view
    "World" top-ui-view
    "Economy" economy-view
    "City" control-ui-view
    ;;"UI Test View" ui-test-view
    ;;"debug" debug-ui-view
    ;;"Old World" basic-ui-view
    ;;"Field" v-view
    ;;"Align" align-ui-view
    ;;"JPanel" jpanel-ui-view
    ;;"Bug" bug-ui-view
    ;;"Map" map-ui-view
    ;;"Economy" economy-ui-view
    ;;"Civ" peep-ui-view
    #_#_
    "Log" messages-ui-view))
