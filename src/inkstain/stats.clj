(ns inkstain.stats
  (:require [io.github.humbleui.ui :as ui]
            [inkstain.peep :as peep]
            [inkstain.systems.movement :as movement]))



(def chassis->stats peep/chassis-size)
(def drive-train->stats movement/drive-train-movement)

(def all-combos
  (into {}
    (map (juxt (juxt :chassis :drive-train) identity))
    (for [c (keys chassis->stats) d (keys drive-train->stats)]
      {:chassis c :drive-train d
       :hp (:hp (peep/chassis-size c))
       :atk (:attack-damage (peep/chassis-size c))
       :spd (:max-speed (movement/effective-drive-train {:chassis c :drive-train d}))
       :acc (:acceleration (movement/effective-drive-train {:chassis c :drive-train d}))})))

(def stat-ranges
  {:hp [(apply min (map :hp (vals all-combos))) (apply max (map :hp (vals all-combos)))]
   :atk [(apply min (map :atk (vals all-combos))) (apply max (map :atk (vals all-combos)))]
   :spd [(apply min (map :spd (vals all-combos))) (apply max (map :spd (vals all-combos)))]
   :acc [(apply min (map :acc (vals all-combos))) (apply max (map :acc (vals all-combos)))]})

(defn val->pips [v [lo hi]]
  (let [frac (/ (- v lo) (max 1 (- hi lo)))]
    (max 1 (min 5 (inc (int (* frac 4)))))))

(defn stat-pips
  ([n]
   [ui/label {:paint {:fill 0xFFCCCCCC}}
    (str (apply str (repeat n "■"))
     (apply str (repeat (- 5 n) "□")))])
  ([label n]
   [ui/row {:gap 4}
    [ui/label {:paint {:fill 0xFF888888}} label]
    [ui/label {:paint {:fill 0xFFCCCCCC}}
     (str (apply str (repeat n "■"))
          (apply str (repeat (- 5 n) "□")))]]))
