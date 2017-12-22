(ns advent-of-code.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn abs [x]
  (Math/abs x))

;; credit to https://www.redblobgames.com/grids/hexagons/#coordinates-cube
(def directions
  {:ne [1 0 -1]
   :nw [-1 1 0]
   :se [1 -1 0]
   :sw [-1 0 1]
   :n  [0 1 -1]
   :s  [0 -1 1]})

(defn cube-distance [a b]
  (->> (map - a b)
       (map abs)
       (apply max)))

(def path (->> (str/split (slurp (io/resource "day-11-input.txt")) #",")
               (map keyword)))

(defn units [coords]
  (map (fn [[start end]]
         (mapv - end start))
       (partition 2 1 coords)))

(defn follow-path [start path]
  (reduce (fn [curr-pos curr-direction]
            (let [curr-coord (get directions curr-direction)]
              (mapv + curr-pos curr-coord)))
          start
          path))

(comment

  ;; Part 1
  (cube-distance [0 0 0] (follow-path [0 0 0] path))

  )