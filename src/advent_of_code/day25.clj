(ns advent-of-code.day25
  (:require [clojure.set :as set]))

(defn set-one [position one?]
  (fn [one?]
    (set/union one? #{position})))

(defn set-zero [position one?]
  (fn [one?]
    (set/difference one? #{position})))

(defn transition-state [{:keys [state position one?] :as world}]
  (condp = state
    :a (if (not (one? position))
         (-> world
             (update :one? (set-one position one?))
             (update :position inc)
             (assoc :state :b))
         (-> world
             (update :one? (set-zero position one?))
             (update :position dec)
             (assoc :state :c)))
    :b (if (not (one? position))
         (-> world
             (update :one? (set-one position one?))
             (update :position dec)
             (assoc :state :a))
         (-> world
             (update :one? (set-one position one?))
             (update :position inc)
             (assoc :state :d)))
    :c (if (not (one? position))
         (-> world
             (update :one? (set-zero position one?))
             (update :position dec)
             (assoc :state :b))
         (-> world
             (update :one? (set-zero position one?))
             (update :position dec)
             (assoc :state :e)))
    :d (if (not (one? position))
         (-> world
             (update :one? (set-one position one?))
             (update :position inc)
             (assoc :state :a))
         (-> world
             (update :one? (set-zero position one?))
             (update :position inc)
             (assoc :state :b)))
    :e (if (not (one? position))
         (-> world
             (update :one? (set-one position one?))
             (update :position dec)
             (assoc :state :f))
         (-> world
             (update :one? (set-one position one?))
             (update :position dec)
             (assoc :state :c)))
    :f (if (not (one? position))
         (-> world
             (update :one? (set-one position one?))
             (update :position inc)
             (assoc :state :d))
         (-> world
             (update :one? (set-one position one?))
             (update :position inc)
             (assoc :state :a)))
    (throw (ex-info "" {:unknown-state state}))))

(comment


  ;; Part 1
  (let [initial-world {:state    :a
                       :position 0
                       :one?     #{}}]
    (->> initial-world
         (iterate transition-state)
         (take (inc 12481997))
         (last)
         (:one?)
         (count)))

  )