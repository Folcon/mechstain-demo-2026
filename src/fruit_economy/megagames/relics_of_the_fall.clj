(ns fruit-economy.megagames.relics-of-the-fall
  (:require [datascript.core :as ds]))


#_
(let [ds-db (ds/db-with (ds/empty-db {:day {:db/index true}
                                      :money {:db/index true}
                                      :kind {:db/index true}
                                      :good {:db/index true}
                                      :place {:db/valueType :db.type/ref}
                                      :coord {:db/unique :db.unique/identity}
                                      :settlement/place {:db/valueType :db.type/ref}
                                      :hometown {:db/valueType :db.type/ref}
                                      :governs {:db/valueType :db.type/ref
                                                :db/cardinality :db.cardinality/one}}))])

(let [resource-tokens [:food :scrap :tech :relic]
      db (ds/db-with (ds/empty-db {:area/name {:db/unique :db.unique/identity}
                                   :cell/name {:db/unique :db.unique/identity}
                                   :area/neighbours {:db/valueType :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                   :cell/neighbours {:db/valueType :db.type/ref
                                                     :db/cardinality :db.cardinality/many}
                                   :area/cell {:db/valueType :db.type/ref
                                               :db/cardinality :db.cardinality/one}})
           (into
             [{:kind :calendar :phase :deployment :season :spring}]
             [{:area/name "Dustforge"}
              {:area/name "The Arch"}
              {:area/name "Chernov's Rest"}
              {:area/name "Tajiki Perch"}
              {:area/name "Deep Rock"}
              {:area/name "Crankgate"}
              {:area/name "Impact City"}
              {:area/name "Eastern Badlands"}
              {:area/name "Indus Highlands"}
              {:area/name "Mountain Heart"}
              {:area/name "High Point"}
              {:area/name "Ilik Hills"}
              {:area/name "Southern Badlands"}
              {:area/name "Near Wilds"}
              {:area/name "Indus Pass"}
              {:area/name "Oleg's Folly"}
              {:area/name "Chaki Foodhills"}
              {:area/name "Eagle's Oasis"}
              {:area/name "Maymahi"}
              {:area/name "Qandari"}
              {:area/name "Kabuli"}
              {:area/name "Chaki Desert"}

              [:db/add [:area/name "Dustforge"] :area/neighbours [:area/name "The Arch"]]
              [:db/add [:area/name "Dustforge"] :area/neighbours [:area/name "Crankgate"]]

              [:db/add [:area/name "The Arch"] :area/neighbours [:area/name "Dustforge"]]
              [:db/add [:area/name "The Arch"] :area/neighbours [:area/name "Crankgate"]]
              [:db/add [:area/name "The Arch"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "The Arch"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "The Arch"] :area/neighbours [:area/name "Chernov's Rest"]]

              [:db/add [:area/name "Chernov's Rest"] :area/neighbours [:area/name "The Arch"]]
              [:db/add [:area/name "Chernov's Rest"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "Chernov's Rest"] :area/neighbours [:area/name "Indus Highlands"]]
              [:db/add [:area/name "Chernov's Rest"] :area/neighbours [:area/name "Tajiki Perch"]]

              [:db/add [:area/name "Tajiki Perch"] :area/neighbours [:area/name "Chernov's Rest"]]
              [:db/add [:area/name "Tajiki Perch"] :area/neighbours [:area/name "Indus Highlands"]]
              [:db/add [:area/name "Tajiki Perch"] :area/neighbours [:area/name "Deep Rock"]]

              [:db/add [:area/name "Crankgate"] :area/neighbours [:area/name "Dustforge"]]
              [:db/add [:area/name "Crankgate"] :area/neighbours [:area/name "The Arch"]]
              [:db/add [:area/name "Crankgate"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Crankgate"] :area/neighbours [:area/name "Southern Badlands"]]
              [:db/add [:area/name "Crankgate"] :area/neighbours [:area/name "Ilik Hills"]]

              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Crankgate"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "The Arch"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Oleg's Folly"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Near Wilds"]]
              [:db/add [:area/name "Impact City"] :area/neighbours [:area/name "Southern Badlands"]]

              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "The Arch"]]
              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "Chernov's Rest"]]
              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "Indus Highlands"]]
              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "Mountain Heart"]]
              [:db/add [:area/name "Eastern Badlands"] :area/neighbours [:area/name "Oleg's Folly"]]

              [:db/add [:area/name "Indus Highlands"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "Indus Highlands"] :area/neighbours [:area/name "Chernov's Rest"]]
              [:db/add [:area/name "Indus Highlands"] :area/neighbours [:area/name "Tajiki Perch"]]
              [:db/add [:area/name "Indus Highlands"] :area/neighbours [:area/name "Deep Rock"]]
              [:db/add [:area/name "Indus Highlands"] :area/neighbours [:area/name "Mountain Heart"]]

              [:db/add [:area/name "Deep Rock"] :area/neighbours [:area/name "Indus Highlands"]]
              [:db/add [:area/name "Deep Rock"] :area/neighbours [:area/name "Tajiki Perch"]]
              [:db/add [:area/name "Deep Rock"] :area/neighbours [:area/name "High Point"]]
              [:db/add [:area/name "Deep Rock"] :area/neighbours [:area/name "Mountain Heart"]]

              [:db/add [:area/name "Ilik Hills"] :area/neighbours [:area/name "Crankgate"]]
              [:db/add [:area/name "Ilik Hills"] :area/neighbours [:area/name "Southern Badlands"]]
              [:db/add [:area/name "Ilik Hills"] :area/neighbours [:area/name "Maymahi"]]

              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Ilik Hills"]]
              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Crankgate"]]
              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Near Wilds"]]
              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Qandari"]]
              [:db/add [:area/name "Southern Badlands"] :area/neighbours [:area/name "Maymahi"]]

              [:db/add [:area/name "Near Wilds"] :area/neighbours [:area/name "Southern Badlands"]]
              [:db/add [:area/name "Near Wilds"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Near Wilds"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Near Wilds"] :area/neighbours [:area/name "Kabuli"]]
              [:db/add [:area/name "Near Wilds"] :area/neighbours [:area/name "Qandari"]]

              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Near Wilds"]]
              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Oleg's Folly"]]
              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Chaki Foodhills"]]
              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Chaki Desert"]]
              [:db/add [:area/name "Indus Pass"] :area/neighbours [:area/name "Kabuli"]]

              [:db/add [:area/name "Oleg's Folly"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Oleg's Folly"] :area/neighbours [:area/name "Impact City"]]
              [:db/add [:area/name "Oleg's Folly"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "Oleg's Folly"] :area/neighbours [:area/name "Mountain Heart"]]
              [:db/add [:area/name "Oleg's Folly"] :area/neighbours [:area/name "Chaki Foodhills"]]

              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "Oleg's Folly"]]
              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "Eastern Badlands"]]
              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "Indus Highlands"]]
              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "Deep Rock"]]
              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "High Point"]]
              [:db/add [:area/name "Mountain Heart"] :area/neighbours [:area/name "Chaki Foodhills"]]

              [:db/add [:area/name "High Point"] :area/neighbours [:area/name "Mountain Heart"]]
              [:db/add [:area/name "High Point"] :area/neighbours [:area/name "Deep Rock"]]
              [:db/add [:area/name "High Point"] :area/neighbours [:area/name "Eagle's Oasis"]]
              [:db/add [:area/name "High Point"] :area/neighbours [:area/name "Chaki Foodhills"]]

              [:db/add [:area/name "Maymahi"] :area/neighbours [:area/name "Ilik Hills"]]
              [:db/add [:area/name "Maymahi"] :area/neighbours [:area/name "Southern Badlands"]]
              [:db/add [:area/name "Maymahi"] :area/neighbours [:area/name "Qandari"]]

              [:db/add [:area/name "Qandari"] :area/neighbours [:area/name "Maymahi"]]
              [:db/add [:area/name "Qandari"] :area/neighbours [:area/name "Southern Badlands"]]
              [:db/add [:area/name "Qandari"] :area/neighbours [:area/name "Near Wilds"]]
              [:db/add [:area/name "Qandari"] :area/neighbours [:area/name "Kabuli"]]

              [:db/add [:area/name "Kabuli"] :area/neighbours [:area/name "Qandari"]]
              [:db/add [:area/name "Kabuli"] :area/neighbours [:area/name "Near Wilds"]]
              [:db/add [:area/name "Kabuli"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Kabuli"] :area/neighbours [:area/name "Chaki Desert"]]

              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "Oleg's Folly"]]
              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "Mountain Heart"]]
              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "High Point"]]
              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "Eagle's Oasis"]]
              [:db/add [:area/name "Chaki Foodhills"] :area/neighbours [:area/name "Chaki Desert"]]

              [:db/add [:area/name "Eagle's Oasis"] :area/neighbours [:area/name "Chaki Foodhills"]]
              [:db/add [:area/name "Eagle's Oasis"] :area/neighbours [:area/name "High Point"]]
              [:db/add [:area/name "Eagle's Oasis"] :area/neighbours [:area/name "Chaki Desert"]]

              [:db/add [:area/name "Chaki Desert"] :area/neighbours [:area/name "Kabuli"]]
              [:db/add [:area/name "Chaki Desert"] :area/neighbours [:area/name "Indus Pass"]]
              [:db/add [:area/name "Chaki Desert"] :area/neighbours [:area/name "Chaki Foodhills"]]
              [:db/add [:area/name "Chaki Desert"] :area/neighbours [:area/name "Eagle's Oasis"]]

              {:cell/name "A1" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A2" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A3" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A4" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A5" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A6" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A7" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A8" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A9" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A10" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A11" :area/cell [:area/name "Dustforge"] :biome :steppe}
              {:cell/name "A12" :area/cell [:area/name "Dustforge"] :biome :steppe}

              {:cell/name "B1" :area/cell [:area/name "The Arch"] :biome :steppe}
              {:cell/name "B2" :area/cell [:area/name "The Arch"] :biome :hills}
              {:cell/name "B5" :area/cell [:area/name "The Arch"] :biome :steppe}

              {:cell/name "E1" :area/cell [:area/name "Crankgate"] :biome :steppe}
              {:cell/name "E2" :area/cell [:area/name "Crankgate"] :biome :steppe}
              {:cell/name "E3" :area/cell [:area/name "Crankgate"] :biome :steppe}
              {:cell/name "E5" :area/cell [:area/name "Crankgate"] :biome :steppe}

              [:db/add [:cell/name "A1"] :cell/neighbours [:cell/name "A2"]]
              [:db/add [:cell/name "A1"] :cell/neighbours [:cell/name "A5"]]
              [:db/add [:cell/name "A1"] :cell/neighbours [:cell/name "A6"]]

              [:db/add [:cell/name "A2"] :cell/neighbours [:cell/name "A1"]]
              [:db/add [:cell/name "A2"] :cell/neighbours [:cell/name "A6"]]
              [:db/add [:cell/name "A2"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A2"] :cell/neighbours [:cell/name "A3"]]

              [:db/add [:cell/name "A3"] :cell/neighbours [:cell/name "A2"]]
              [:db/add [:cell/name "A3"] :cell/neighbours [:cell/name "A4"]]
              [:db/add [:cell/name "A3"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A3"] :cell/neighbours [:cell/name "A8"]]

              [:db/add [:cell/name "A4"] :cell/neighbours [:cell/name "B1"]]
              [:db/add [:cell/name "A4"] :cell/neighbours [:cell/name "B2"]]
              [:db/add [:cell/name "A4"] :cell/neighbours [:cell/name "A8"]]
              [:db/add [:cell/name "A4"] :cell/neighbours [:cell/name "A3"]]

              [:db/add [:cell/name "A5"] :cell/neighbours [:cell/name "A1"]]
              [:db/add [:cell/name "A5"] :cell/neighbours [:cell/name "A6"]]
              [:db/add [:cell/name "A5"] :cell/neighbours [:cell/name "A9"]]
              [:db/add [:cell/name "A5"] :cell/neighbours [:cell/name "A10"]]

              [:db/add [:cell/name "A6"] :cell/neighbours [:cell/name "A1"]]
              [:db/add [:cell/name "A6"] :cell/neighbours [:cell/name "A2"]]
              [:db/add [:cell/name "A6"] :cell/neighbours [:cell/name "A5"]]
              [:db/add [:cell/name "A6"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A6"] :cell/neighbours [:cell/name "A9"]]

              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A2"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A3"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A6"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A8"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A9"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "A12"]]
              [:db/add [:cell/name "A7"] :cell/neighbours [:cell/name "B5"]]

              [:db/add [:cell/name "A8"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A8"] :cell/neighbours [:cell/name "A3"]]
              [:db/add [:cell/name "A8"] :cell/neighbours [:cell/name "A4"]]
              [:db/add [:cell/name "A8"] :cell/neighbours [:cell/name "B2"]]
              [:db/add [:cell/name "A8"] :cell/neighbours [:cell/name "B5"]]

              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A5"]]
              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A6"]]
              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A12"]]
              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A11"]]
              [:db/add [:cell/name "A9"] :cell/neighbours [:cell/name "A10"]]

              [:db/add [:cell/name "A10"] :cell/neighbours [:cell/name "A5"]]
              [:db/add [:cell/name "A10"] :cell/neighbours [:cell/name "A9"]]
              [:db/add [:cell/name "A10"] :cell/neighbours [:cell/name "A11"]]
              [:db/add [:cell/name "A10"] :cell/neighbours [:cell/name "E1"]]
              [:db/add [:cell/name "A10"] :cell/neighbours [:cell/name "E2"]]

              [:db/add [:cell/name "A11"] :cell/neighbours [:cell/name "A10"]]
              [:db/add [:cell/name "A11"] :cell/neighbours [:cell/name "A9"]]
              [:db/add [:cell/name "A11"] :cell/neighbours [:cell/name "A12"]]
              [:db/add [:cell/name "A11"] :cell/neighbours [:cell/name "E2"]]
              [:db/add [:cell/name "A11"] :cell/neighbours [:cell/name "E3"]]

              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "A7"]]
              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "A9"]]
              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "A11"]]
              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "B5"]]
              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "E3"]]
              [:db/add [:cell/name "A12"] :cell/neighbours [:cell/name "E5"]]


              {:role :elder :team :wilder}
              {:role :mech-ranger :team :wilder}
              {:role :engine-witch :team :wilder}
              {:role :omen-seer :team :wilder}
              {:role :imperial-prefect :team :steel-empire}
              {:role :imperial-lictor :team :steel-empire}
              {:role :imperial-legate :team :steel-empire}
              {:role :imperial-legate :team :steel-empire}
              {:role :plutarchs :team :silk-city}]))]
  (ds/q
    '[:find (pull ?a [*]) (pull ?b [*])
      :where
      [?a :area/neighbours ?b]]
    db))
