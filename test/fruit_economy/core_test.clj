(ns fruit-economy.core-test
  (:require [clojure.test :refer :all]
            [fruit-economy.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(-> (land/make-land "World" width height)
  (land/gen-land)
  (land/populate 50 #_100)
  (land/spawn-units 10)
  (economy/add-resources)
  (civ/try-spawn-new-civs 10))