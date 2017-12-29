(ns advent-of-code.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (slurp (io/resource "day-20-input.txt")))

(defn parse-nums [nums]
  (->> (str/split nums #",")
       (mapv #(Integer/parseInt %))))

(defn parse-elem [elem]
  (let [re #"(\w)=<(.+)>"
        [[_ a nums]] (re-seq re elem)]
    [(keyword a) (parse-nums nums)]))

(defn parse-line [id line]
  (let [m (->> (str/split line #", ")
               (map parse-elem)
               (into {}))]
    (assoc m :id id)))

(defn input->particles [input]
  (->> (str/split-lines input)
       (map-indexed #(parse-line %1 %2))))

(defn update-particle [{:keys [p v a] :as particle}]
  (let [v' (mapv + v a)
        p' (mapv + p v')]
    (assoc particle :v v' :p p')))

(defn particle-positions [n particles]
  (loop [particles' particles
         curr 1]
    (if (> curr n)
      particles'
      (recur (mapv update-particle particles')
             (inc curr)))))

(defn remove-collisions [particles]
  (->> (group-by :p particles)
       (filter (fn [[p particles]]
                 (= (count particles) 1)))
       (mapcat second)))

(defn particle-positions-with-collisions [n particles]
  (loop [particles' particles
         curr 1]
    (if (> curr n)
      particles'
      (recur (remove-collisions (mapv update-particle particles'))
             (inc curr)))))

(defn abs [x]
  (Math/abs x))

(defn distance-to-origin [{:keys [p] :as particle}]
  (assoc particle :distance-to-origin (reduce + (map abs p))))

(comment

  ;; Part 1
  (->> (input->particles input)
       (particle-positions 1000)
       (map distance-to-origin)
       (sort-by :distance-to-origin)
       (first)
       (:id))

  ;; Part 2
  (->> (input->particles input)
       (particle-positions-with-collisions 1000)
       (count))

  )