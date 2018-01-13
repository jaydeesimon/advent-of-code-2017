(ns advent-of-code.day25
  (:require [clojure.set :as set]))

(defn set-one [position ones]
  (fn [ones]
    (set/union ones #{position})))

(defn set-zero [position ones]
  (fn [ones]
    (set/difference ones #{position})))

(defn transition-state [{:keys [state position ones] :as world}]
  (condp = state
    :a (if (not (ones position))
         (-> world
             (update :ones (set-one position ones))
             (update :position inc)
             (assoc :state :b))
         (-> world
             (update :ones (set-zero position ones))
             (update :position dec)
             (assoc :state :c)))
    :b (if (not (ones position))
         (-> world
             (update :ones (set-one position ones))
             (update :position dec)
             (assoc :state :a))
         (-> world
             (update :ones (set-one position ones))
             (update :position inc)
             (assoc :state :d)))
    :c (if (not (ones position))
         (-> world
             (update :ones (set-zero position ones))
             (update :position dec)
             (assoc :state :b))
         (-> world
             (update :ones (set-zero position ones))
             (update :position dec)
             (assoc :state :e)))
    :d (if (not (ones position))
         (-> world
             (update :ones (set-one position ones))
             (update :position inc)
             (assoc :state :a))
         (-> world
             (update :ones (set-zero position ones))
             (update :position inc)
             (assoc :state :b)))
    :e (if (not (ones position))
         (-> world
             (update :ones (set-one position ones))
             (update :position dec)
             (assoc :state :f))
         (-> world
             (update :ones (set-one position ones))
             (update :position dec)
             (assoc :state :c)))
    :f (if (not (ones position))
         (-> world
             (update :ones (set-one position ones))
             (update :position inc)
             (assoc :state :d))
         (-> world
             (update :ones (set-one position ones))
             (update :position inc)
             (assoc :state :a)))
    (throw (ex-info "" {:unknown-state state}))))

(comment


  ;; Part 1
  (let [initial-world {:state    :a
                       :position 0
                       :ones     #{}}]
    (->> initial-world
         (iterate transition-state)
         (take (inc 12481997))
         (last)
         (:ones)
         (count)))

  )