(ns advent-of-code.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn dims [lines]
  (let [width (apply max (map count lines))
        height (count lines)]
    [width height]))

(defn coords [width height]
  (for [r (range height)
        c (range width)]
    [r c]))

(defn create-grid [lines width height]
  (let [gridv (mapv vec lines)]
    (reduce (fn [gridm coord]
              (let [v (get-in gridv coord)]
                (if (and (not (nil? v)) (not= \space v))
                  (assoc gridm coord v)
                  gridm)))
            {}
            (coords width height))))

(defn input->world [input]
  (let [lines (str/split-lines input)
        [width height] (dims lines)
        grid (create-grid lines width height)]
    {:grid   grid
     :width  width
     :height height
     :start  (ffirst (filter (fn [[[r _] v]]
                              (and (zero? r) (= v \|)))
                            grid))}))

(defn letter? [c]
  (Character/isLetter ^Character c))

(defn find-new-direction [world coord path]
  (let [candidate-coords (map #(mapv + coord %) [[1 0] [-1 0] [0 -1] [0 1]])
        next-coords (filter (fn [candidate-coord]
                             (and (get-in world [:grid candidate-coord])
                                  (not ((set path) candidate-coord))))
                           candidate-coords)
        _ (when (> (count next-coords) 1) (throw (ex-info "" {:too-many next-coords})))
        next-coord (first next-coords)]
    [next-coord (mapv - next-coord coord)]))



(comment

  ;; Part 1
  (let [world (input->world (slurp (io/resource "day-19-input.txt")))]
    (loop [coord (:start world)
           path []
           letters []
           direction [1 0]]
      (let [curr-tile-type (get-in world [:grid coord])
            next-coord (mapv + coord direction)
            next-tile-type (get-in world [:grid next-coord])]
        (cond

          (get-in world [:grid next-coord])
          (recur next-coord
                 (conj path coord)
                 (if (letter? curr-tile-type)
                   (conj letters curr-tile-type)
                   letters)
                 direction)

          (= \+ curr-tile-type)
          (let [[next-coord new-direction] (find-new-direction world coord path)]
            (if (nil? next-coord)
              {:path    path
               :letters letters}
              (recur next-coord
                     (conj path coord)
                     letters
                     new-direction)))

          :else
          {:path (conj path coord)
           :letters (if (letter? curr-tile-type)
                      (conj letters curr-tile-type)
                      letters)}))))

  )