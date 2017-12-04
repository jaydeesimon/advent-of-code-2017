(ns advent-of-code.day03
  (:require [clojure.java.io :as io]))

(def directions->coord {:up    [0 1]
                        :left  [-1 0]
                        :down  [0 -1]
                        :right [1 0]})

(defn one-swirl [n]
  (let [first-leg (dec (* 2 n))]
    [[1 :right]
     [first-leg :up]
     [(inc first-leg) :left]
     [(inc first-leg) :down]
     [(inc first-leg) :right]]))

(defn expand-move [[n direction]]
  (mapv (constantly direction) (range n)))

(defn swirl [n]
  (->> (range 1 (inc n))
       (mapcat one-swirl)
       (mapcat expand-move)))

(defn swirl-coords [start n]
  (reduce (fn [coords direction]
            (let [last-coord (last coords)
                  coord-direction (directions->coord direction)
                  new-coord (mapv + last-coord coord-direction)]
              (conj coords new-coord)))
          [start]
          (swirl n)))

(defn abs [x]
  (Math/abs x))

(defn manhattan-distance [[p1 p2] [q1 q2]]
  (+ (abs (- p1 q1)) (abs (- p2 q2))))

(comment

  (take 1 (read-string (slurp (io/resource "swirl-coords.edn"))))


  (swirl 257)

  (->> (map (fn [coord n]
              [coord n])
            (rest (range))
            (read-string (slurp (io/resource "swirl-coords.edn"))))
       (take 265149)
       (last))

  ;; part one
  (manhattan-distance [0 0] [181 -257])

  (one-swirl 1)
  (one-swirl 2)

  )