(ns fruit-economy.fruit-economy.sim.market-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [fruit-economy.sim.market :refer [empty-order-book load-orders match-orders]]))


;;(defn empty-level []
;;  (with-meta {} {:volume 0}))
;;
;;(defn level-size [price-level]
;;  (:volume (meta price-level)))
;;
;;(defn empty-buys [] (sorted-map-by (comp - compare)))
;;(defn empty-sells [] (sorted-map))
;;
;;(defn empty-order-book []
;;  {;; order from cheap => expensive
;;   :buys  (empty-buys)
;;   ;; order from expensive => cheap
;;   :sells (empty-sells)})
;;
;;(defn insert [order-book {:keys [side price size id] :as order}]
;;  (update-in order-book [side price]
;;    (fn [level]
;;      (-> (or level (empty-level))
;;        (assoc id order)
;;        (vary-meta update :volume + size)))))
;;
;;(defn delete [order-book {:keys [side price id] :as order}]
;;  (let [size (get-in order-book [side price id :size])
;;        lvl-sz (level-size (get-in order-book [side price]))]
;;    (if (= size lvl-sz)
;;      (update order-book side dissoc price)
;;      (cond-> order-book
;;        size
;;        (update-in [side price]
;;          (fn [level]
;;            (-> level
;;              (dissoc order)
;;              (vary-meta update :volume - size))))))))
;;
;;(defn aggregate-order-book
;;  "gives volume at different prices"
;;  [order-book]
;;  (->> order-book
;;    (map (fn [[side-key price->level]]
;;           [side-key
;;            (reduce-kv
;;              (fn [m price level]
;;                (assoc m price (level-size level)))
;;              (empty price->level)
;;              price->level)]))
;;    (into {})))


(def small-buyer {:price 1 :size 1 :side :buys :id :small-buyer})
(def medium-buyer {:price 1 :size 2 :side :buys :id :medium-buyer})
(def rich-buyer {:price 2 :size 1 :side :buys :id :rich-buyer})
(def small-seller {:price 1 :size 1 :side :sell :id :small-seller})
(def medium-seller {:price 1 :size 2 :side :sell :id :medium-seller})
(def pricey-seller {:price 1.5 :size 2 :side :sell :id :pricy-seller})
(def large-seller {:price 2 :size 3 :side :sell :id :large-seller})


(deftest orders-add-on-correct-side-test
  (testing "Testing orders are added to the correct side of the order book"
    (let [orders [small-buyer rich-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)]
      (is (= 2 (count (:buys order-book))))
      (is (= 1 (count (:sell order-book)))))))

(deftest orders-prioritised-by-price-test
  (testing "cheaper sells first and more expensive buys first"
    (let [orders [small-buyer medium-buyer rich-buyer small-seller medium-seller large-seller]
          order-book (load-orders (empty-order-book) orders)]
      (is (= [[:small-seller {:price 1, :size 1, :side :sell, :id :small-seller}]
              [:medium-seller {:price 2, :size 1.5, :side :sell, :id :medium-seller}]
              [:large-seller {:price 3, :size 2, :side :sell, :id :large-seller}]]
             (reduce conj [] (:sell order-book))))
      (is (= [[:rich-buyer {:price 2, :size 1, :side :buys, :id :rich-buyer}]
              [:small-buyer {:price 1, :size 1, :side :buys, :id :small-buyer}]
              [:medium-buyer {:price 1, :size 2, :side :buys, :id :medium-buyer}]]
             (reduce conj [] (:buys order-book)))))))

(deftest orders-matched-test
  (testing "Test equal orders on opposite sides match"
    (let [orders [small-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sell order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :small-seller
               :buyer :small-buyer
               :buy-order {:price 1 :size 1 :side :buys :id :small-buyer}
               :sell-order {:price 1 :size 1 :side :sell :id :small-seller}}]
             (:matched order-book')))))

  (testing "Test equal sized orders with different prices on opposite sides match"
    (let [orders [rich-buyer small-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sell order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :small-seller
               :buyer :rich-buyer
               :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
               :sell-order {:price 1 :size 1 :side :sell :id :small-seller}}]
             (:matched order-book')))))

  (testing "Test multiple orders can match as long as all buyer prices are over seller prices"
    (let [orders [small-buyer rich-buyer
                  medium-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (zero? (count (:buys order-book'))))
      (is (zero? (count (:sell order-book'))))
      (is (= [{:price 1
               :size 1
               :seller :medium-seller
               :buyer :rich-buyer
               :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
               :sell-order {:price 1 :size 2 :side :sell :id :medium-seller}}
              {:price 1
               :size 1
               :seller :medium-seller
               :buyer :small-buyer
               :buy-order {:price 1 :size 1 :side :buys :id :small-buyer}
               :sell-order {:price 1 :size 1 :side :sell :id :medium-seller}}]
             (:matched order-book')))))

  (testing "Test sellers with too high a price will not find buyers"
    (let [orders [medium-buyer
                  pricey-seller]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (= 1 (count (:buys order-book'))))
      (is (= 1 (count (:sell order-book'))))
      (is (= []
             (:matched order-book'))))))

(deftest cannot-self-trade-test
  (testing "orders cannot be matched with the same buyer and seller"
    (let [orders [{:price 1 :size 1 :side :buys :id :self-trader} {:price 1 :size 1 :side :sell :id :self-trader}]
          order-book (load-orders (empty-order-book) orders)
          order-book' (match-orders order-book)]
      (is (= []
            (:matched order-book'))))))


(def orders-gen
  (gen/vector (gen/hash-map
                :price gen/nat
                :size gen/nat
                :side (gen/elements [:buys :sell])
                :id (gen/one-of [gen/keyword gen/small-integer]))))

(def order-book-gen
  (gen/fmap
    (fn [v]
      (load-orders (empty-order-book) v))
    orders-gen))

#_
(defspec market-match-does-not-lose-orders-prop
  (prop/for-all [market order-book-gen]
    (= (into [] cat ((juxt (comp vals :buys) (comp vals :sell) :matched) market)))))

#_
(let [market (gen/generate (gen/fmap
                             (fn [v]
                               (load-orders (empty-order-book) v))
                             (gen/vector (gen/hash-map
                                           :price gen/nat
                                           :size gen/nat
                                           :side (gen/elements [:buys :sell])
                                           :id (gen/one-of [gen/keyword gen/small-integer])))))]
  (->> (match-orders market)
    ((juxt :buys :sell :matched))
    ((fn [[buys sells matched]]
       (into [])))
    (into [] (comp (map (juxt :buy-order :sell-order)) cat)))
  #_(match-orders market)
  #_(let [_ (println :market market)
          market' (match-orders market)
          unfilled-orders (into [] cat ((juxt (comp vals :buys) (comp vals :sell)) market'))]
      (= ((juxt :buys :sell) market')
        ((juxt :buys :sell) (->> unfilled-orders (load-orders (empty-order-book)) (match-orders))))))


(defspec market-match-idempotent-prop
  ;; Testing that there are no further matches to be made once matched
  (prop/for-all [market order-book-gen]
    (let [market' (match-orders market)
          unfilled-orders (into [] cat ((juxt (comp vals :buys) (comp vals :sell)) market'))]
      (= ((juxt :buys :sell) market')
        ((juxt :buys :sell) (->> unfilled-orders (load-orders (empty-order-book)) (match-orders)))))))

(market-match-idempotent-prop)

;;(defspec market-sort-idempotent-prop
;;  ;; Testing that
;;  (prop/for-all [market (gen/hash-map
;;                          :buy-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer))
;;                          :sell-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer)))]
;;    (= (sort-orders market) (sort-orders (sort-orders market)))))
;;
;;(market-sort-idempotent-prop)
;;
;;(identity medium-buyer)
;;
;;(->> (into [] cat ((juxt (comp vals :buys) (comp vals :sells)) (match-orders
;;                                                                 (gen/generate
;;                                                                   (gen/fmap
;;                                                                     (fn [v]
;;                                                                       (load-orders (empty-order-book) v))
;;                                                                     (gen/vector (gen/hash-map
;;                                                                                   :price gen/nat
;;                                                                                   :size gen/nat
;;                                                                                   :side (gen/elements [:buys :sells])
;;                                                                                   :id (gen/one-of [gen/keyword gen/small-integer]))))))))
;;  (load-orders (empty-order-book)) match-orders)


(testing ""
  (let [orders [small-buyer rich-buyer
                pricey-seller]
        order-book (load-orders (empty-order-book) orders)
        order-book' (match-orders order-book)]
    (is (= 1 (count (:buys order-book'))))
    (is (= 1 (count (:sell order-book'))))
    (is (= [{:price 1.5
             :size 1
             :seller :pricy-seller
             :buyer :rich-buyer
             :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
             :sell-order {:price 1.5 :size 2 :side :sell :id :pricy-seller}}]
           (:matched order-book')))))

(testing ""
  (let [orders [small-buyer rich-buyer
                pricey-seller]
        order-book (load-orders (empty-order-book) orders)
        order-book' (match-orders order-book)]
    (is (= 1 (count (:buys order-book'))))
    (is (= 1 (count (:sell order-book'))))
    (is (= [{:price 1.5
             :size 1
             :seller :pricy-seller
             :buyer :rich-buyer
             :buy-order {:price 2 :size 1 :side :buys :id :rich-buyer}
             :sell-order {:price 1.5 :size 2 :side :sell :id :pricy-seller}}]
          (:matched order-book')))))


;;(defn sort-orders [market]
;;  (-> market
;;    ;; order from cheap => expensive
;;    (update :buy-orders (comp vec (partial sort-by :unit-price <)))
;;    ;; order from expensive => cheap
;;    (update :sell-orders (comp vec (partial sort-by :unit-price >)))))
;;
;;#_
;;(let [resource :food
;;      add-order (fn [market {:keys [kind] :as order}]
;;                  (condp = kind
;;                    :buy (update market :buy-orders conj order)
;;                    :sell (update market :sell-orders conj order)))
;;      #_
;;      {:resource resource
;;       :sell-orders [{:unit-price 1 :id 0} {:unit-price 2 :id 2} {:unit-price 1 :id 3} {:unit-price 1 :id 4}]
;;       :buy-orders [{:unit-price 1 :id 0} {:unit-price 2 :id 2} {:unit-price 1 :id 3} {:unit-price 1 :id 4}]
;;       :completed-orders []
;;       :settle-price 0}
;;      match-orders (fn [{:keys [buy-orders sell-orders] :as market}]
;;                     (println :market market)
;;                     (if (or
;;                           (not (seq buy-orders))
;;                           (not (seq sell-orders)))
;;                       ;; nothing to match against
;;                       market
;;
;;                       #_(loop [buy-orders buy-orders sell-orders sell-orders]
;;                           (recur))))]
;;  (-> {:resource resource
;;       :sell-orders [{:unit-price 1 :id 0 :quantity 1} {:unit-price 2 :id 2 :quantity 1} {:unit-price 1 :id 3 :quantity 1} {:unit-price 1 :id 4 :quantity 1}]
;;       :buy-orders [{:unit-price 1 :id 0 :quantity 1} {:unit-price 2 :id 2 :quantity 1} {:unit-price 1 :id 3 :quantity 1} {:unit-price 1 :id 4 :quantity 1}]
;;       :completed-orders []
;;       :settle-price 0}
;;    (sort-orders)
;;    #_(match-orders)))
;;
;;(let [market {:resource :food,
;;              :sell-orders [{:unit-price 2, :id 2, :quantity 1}
;;                            {:unit-price 1, :id 0, :quantity 1}
;;                            {:unit-price 1, :id 3, :quantity 1}
;;                            {:unit-price 1, :id 4, :quantity 2}],
;;              :buy-orders [{:unit-price 1, :id 0, :quantity 1}
;;                           {:unit-price 1, :id 3, :quantity 1}
;;                           {:unit-price 1, :id 4, :quantity 1}
;;                           {:unit-price 2, :id 2, :quantity 1}],
;;              :completed-orders [],
;;              :settle-price 0}
;;      {:keys [sell-orders buy-orders]} market]
;;  (let [{sell-unit-price :unit-price sell-quantity :quantity :as sell-order} (peek sell-orders)
;;        {buy-unit-price :unit-price buy-quantity :quantity :as buy-order} (peek buy-orders)]
;;    (if (< buy-unit-price sell-unit-price)
;;      market
;;      (let [price sell-unit-price]
;;        (if (> sell-quantity buy-quantity)
;;          (-> market
;;            (update :completed-orders conj buy-order)
;;            (update :buy-orders pop))))))
;;  #_(loop [buy-orders buy-orders sell-orders sell-orders]
;;      (recur)))
;;
;;(defspec market-sort-idempotent-prop
;;  ;; Testing that
;;  (prop/for-all [market (gen/hash-map
;;                          :buy-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer))
;;                          :sell-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer)))]
;;    (= (sort-orders market) (sort-orders (sort-orders market)))))
;;
;;(market-sort-idempotent-prop)
;;
;;;;{:from from :buy buying :price price}
;;
;;(deftest market-sort-test)
;;(testing "Testing equal order sizes match"
;;  (let [market {:sell-orders [{:unit-price 1 :quantity 2}]
;;                :buy-orders [{:unit-price 1 :quantity 1}]
;;                :completed-orders []}]
;;    (loop [market market
;;           limit 0]
;;      (println :market market)
;;      (let [{:keys [sell-orders buy-orders]} market
;;            {sell-price :unit-price sell-quantity :quantity :as sell-order} (peek sell-orders)
;;            {buy-price :unit-price buy-quantity :quantity :as buy-order} (peek buy-orders)]
;;        (if (or
;;              (or (not (seq buy-orders))
;;                  (not (seq sell-orders)))
;;              (< buy-price sell-price)
;;              (> limit 100))
;;          market
;;
;;          ;; stuff to sell people are willing to buy
;;          (let [price sell-price]
;;            (cond
;;              ;; more to sell
;;              (> sell-quantity buy-quantity)
;;              (recur
;;                (-> market
;;                  (update :completed-orders conj {:buy buy-order :sell sell-order :quantity buy-quantity})
;;                  (update :buy-orders pop))
;;                (inc limit))
;;
;;              (= sell-quantity buy-quantity)
;;              (recur
;;                (-> market
;;                  (update :buy-orders pop)
;;                  (update :sell-orders pop)
;;                  (update :completed-orders conj {:buy buy-order :sell sell-order}))
;;                (inc limit)))))))))
;;
;;#_
;;(let [market {:sell-orders [{:unit-price 1 :quantity 2}]
;;              :buy-orders [{:unit-price 1 :quantity 1}]
;;              :completed-orders []}]
;;  (let [{:keys [sell-orders buy-orders]} market]
;;    (loop [market (-> market
;;                    (update :buy-orders pop)
;;                    (update :sell-orders pop))
;;
;;           {buy-price :unit-price buy-quantity :quantity :as buy-order} (peek buy-orders)
;;           {sell-price :unit-price sell-quantity :quantity :as sell-order} (peek sell-orders)
;;           limit 0]
;;      (cond
;;        (or
;;         (not (seq buy-orders))
;;         (not (seq sell-orders))
;;         (< buy-price sell-price)
;;         (> limit 100))
;;        market
;;
;;        ;; more to sell
;;        (> sell-quantity buy-quantity)
;;        (let [sell-order' (assoc sell-order :quantity (- sell-quantity buy-quantity))]
;;          (recur
;;            (update market :completed-orders conj {:buy buy-order :sell sell-order :quantity buy-quantity})
;;            (inc limit)))))))
;;
;;(def sort-idempotent-prop
;;  (prop/for-all [market (gen/hash-map
;;                          :buy-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer))
;;                          :sell-orders
;;                          (gen/vector
;;                            (gen/hash-map
;;                              :unit-price gen/small-integer)))]
;;    (= (sort-orders market) (sort-orders (sort-orders market)))))
;;
;;
;;
;;
;;#_(let [world-data [{:food/price 1 :food/demand 1 :food/supply 1
;;                     :clothes/price 1 :clothes/demand 1 :clothes/supply 1
;;                     :labour/supply 1}
;;                    {:money 1000 :food 10 :clothes 10 :min-food 4 :min-clothes 2 :hometown 1 :planning 15}]
;;        db (d/db-with (d/empty-db peep-shop-schema)
;;             world-data)]
;;    (is (set/subset?
;;          (set [[:db/add 2 :food 10] [:db/add 2 :clothes 10]])
;;          (set (db->datoms (basic/infer db [basic/peep-shop-rule] 1))))))
