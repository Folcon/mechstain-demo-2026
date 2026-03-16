(ns fruit-economy.odoyle
  (:require [odoyle.rules :as o]))


(comment
  (let [rules (o/ruleset
                {:v/temperature-record
                 [:what
                  [id :v/temperature temperature]
                  [id :v/location location]
                  :then-finally
                  (->> (o/query-all o/*session* :v/temperature-record)
                       (o/insert o/*session* :v/derived :v/all-temperatures)
                       o/reset!)]})
        session (-> (reduce o/add-rule (o/->session) rules)
                  (o/insert 1 {:v/temperature -10 :v/location "MCI"})
                  (o/fire-rules))]
    (o/query-all session :v/all-temperatures)))
