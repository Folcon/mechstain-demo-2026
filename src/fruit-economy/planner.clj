(ns fruit-economy.planner
  (:require [datascript.core :as d]))


(def goal
  {:action (fn default-goal [peep world] peep)
   :parent nil})

(def brain
  {:goals []
   :on-tick ()})

(defn clear-completed-goals [goals]
  (loop [goals goals]
    (let [{:keys [finished?] :as current-goal} (peek goals)]
      (if finished?
        (recur (pop goals))
        goals))))

(defn make-goal
  ([name action] (make-goal name action nil))
  ([name action parent]
   {:name name
    :action action
    :parent parent}))

(def idle-goal
  (make-goal
    :idle-goal
    (fn idle-goal [peep _world]
      (println :idle-goal peep)
      peep)))

(def bored-goal
  (make-goal
    :bored-goal
    (fn bored-goal [peep world]
      (println :bored-goal peep)
      (let [{:keys [bored-goals]} peep
            next-goal (cond
                        ;; check for anything on peep that responds to boredom
                        bored-goals (rand-nth bored-goals)
                        ;; try to kill someone or scheme
                        nil nil
                        ;; is there an peep idle behaviour we can apply?
                        nil nil

                        :else idle-goal)]
        (println :next-goal next-goal)
        (-> peep
          (update :goals pop)
          (update :goals conj next-goal))))))

(defn goal-tick [{:keys [goals] :as peep} {:keys [tick] :as world}]
  (println :goal-tick tick :goals goals)
  (let [#_#_goals (clear-completed-goals goals)
        {:keys [action] :as goal} (peek goals)]
    (if goal
      (do
        (println :action tick action (action peep world) :goals goals)
        (-> peep
          (action world)))
      (assoc peep :goals [bored-goal]))))

(comment

  ;; Think about carousing
  ;; IE Adventurer is bored:
  ;;   they go carousing
  ;;   hunt for a quest on the quest board in the tavern
  ;;    - find a monster to kill for bounty
  ;;    - hunt for something lost
  ;;    - harvest some stuff
  ;;    - start a heist to steal something

  ;; Get peep logic working, ie, use this mechanism to model:
  ;; 1) peep leaders running out of stuff to do and then making new decisions
  ;; 2) peep supervisor making decisions
  ;; 3) peep workers doing work
  (let [goals (mapv (partial merge goal) [] #_[{:name :a} {:name :b} {:name :c}])
        peep {:goals goals
              :bored-goals
              [(make-goal :carousing-goal (fn carousing-goal [peep world] (println :carousing-goal peep) peep))
               (make-goal :hunt-monster-goal (fn hunt-monster-goal [peep world] (println :hunt-monster-goal peep) peep))
               (make-goal :harvest-goal (fn harvest-goal [peep world] (println :harvest-goal peep) peep))
               (make-goal :heist-goal (fn heist-goal [peep world] (println :heist-goal peep) peep))]}
        building {:name "pub"}
        world {:peeps [peep]
               :buildings [building]}]
    (-> peep
      (goal-tick (assoc world :tick 0))
      (goal-tick (assoc world :tick 1))
      (goal-tick (assoc world :tick 2))
      (goal-tick (assoc world :tick 3))))

  (d/db-with (d/empty-db)
    [{:name "Peep 1" :on-bored (fn [peep world])}]))
