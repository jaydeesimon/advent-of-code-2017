(ns advent-of-code.day03
  (:require [clojure.java.io :as io]))

(def directions->coord {:up    [0 1]
                        :left  [-1 0]
                        :up-left [-1 1]
                        :up-right [1 1]
                        :down  [0 -1]
                        :right [1 0]
                        :down-left [-1 -1]
                        :down-right [1 -1]})

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

;; this is pretty slow but I only need it to run once
;; i'm sure there's a mathy way to do this but this works for now
;; I saved the output of the swirl coords in an edn file which
;; I can reuse later
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

(defn part-two [swirl-coords stop-n]
  (reduce (fn [coord-sums coord]
            (let [adjacent-coords (mapv #(mapv + coord %) (vals directions->coord))
                  vs (map #(get coord-sums % 0) adjacent-coords)
                  new-sum (reduce + vs)]
              (if (> new-sum stop-n)
                (reduced (assoc coord-sums coord new-sum))
                (assoc coord-sums coord new-sum))))
          {[0 0] 1}
          swirl-coords))

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

  ;; part two
  (->> (part-two (rest (read-string (slurp (io/resource "swirl-coords.edn")))) 265149)
       (vals)
       (apply max))

  (one-swirl 1)
  (one-swirl 2)

  )