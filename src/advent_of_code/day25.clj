(ns advent-of-code.day25
  (:require [clojure.set :as set]))

(defn transition-state [state]
  (condp = (:state state)
    :a (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current inc)
             (assoc :state :b))
         (-> state
             (update :ones (fn [ones]
                             (set/difference ones #{(:current state)})))
             (update :current dec)
             (assoc :state :c)))
    :b (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current dec)
             (assoc :state :a))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current inc)
             (assoc :state :d)))
    :c (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/difference ones #{(:current state)})))
             (update :current dec)
             (assoc :state :b))
         (-> state
             (update :ones (fn [ones]
                             (set/difference ones #{(:current state)})))
             (update :current dec)
             (assoc :state :e)))
    :d (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current inc)
             (assoc :state :a))
         (-> state
             (update :ones (fn [ones]
                             (set/difference ones #{(:current state)})))
             (update :current inc)
             (assoc :state :b)))
    :e (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current dec)
             (assoc :state :f))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current dec)
             (assoc :state :c)))
    :f (if (not ((:ones state) (:current state)))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current inc)
             (assoc :state :d))
         (-> state
             (update :ones (fn [ones]
                             (set/union ones #{(:current state)})))
             (update :current inc)
             (assoc :state :a)))))

(comment


  ;; Part 1
  (let [initial-state {:state :a
                       :current 0
                       :ones #{}}]
    (->> initial-state
         (iterate transition-state)
         (take (inc 12481997))
         (last)
         (:ones)
         (count)))

  )