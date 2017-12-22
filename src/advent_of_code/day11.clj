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

;; used this to make sure I had the direction offsets right
(defn units [coords]
  (map (fn [[start end]]
         (mapv - end start))
       (partition 2 1 coords)))

(defn follow-path [start path]
  (reduce (fn [positions curr-direction]
            (let [curr-coord (get directions curr-direction)]
              (conj positions (mapv + (last positions) curr-coord))))
          [start]
          path))

(comment

  ;; Part 1
  (let [start [0 0 0]
        final-pos (last (follow-path start path))]
    (cube-distance start final-pos))

  ;; Part 2
  (let [start [0 0 0]]
    (->> (map cube-distance (repeat start) (follow-path start path))
         (apply max)))

  )