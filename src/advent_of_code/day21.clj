(ns advent-of-code.day21
  (:require [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]))

(def initial-pattern [[\. \# \.]
                      [\. \. \#]
                      [\# \# \#]])

(def input (slurp (io/resource "day-21-input.txt")))
(def test-input "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#")

(defn rotate-cw [matrix]
  (apply mapv (comp vec reverse vector) matrix))

(defn flip-vertical [matrix]
  (vec (reverse matrix)))

(defn flip-horizontal [matrix]
  (mapv (comp vec reverse) matrix))

(defn variations [matrix]
  (set/union
    (set (take 4 (iterate rotate-cw matrix)))
    (set (take 4 (iterate rotate-cw (flip-vertical matrix))))
    (set (take 4 (iterate rotate-cw (flip-horizontal matrix))))))

(defn parse-enhancement-rule [line]
  (let [[l r] (str/split line #" => ")
        l (mapv vec (str/split l #"/"))
        r (mapv vec (str/split r #"/"))]
    [l r]))

(defn parse-enhancement-rules [input]
  (->> (str/split-lines input)
       (map parse-enhancement-rule)
       (mapcat (fn [[left right]]
                 (map #(vector % right) (variations left))))
       (into {})))

(defn square-offsets [n]
  (for [r (range n) c (range n)]
    [r c]))

(defn origin-coords [big-n small-n]
  (->> (for [r (range big-n) c (range big-n)] [r c])
       (filter (fn [[r c]]
                 (and (zero? (mod r small-n))
                      (zero? (mod c small-n)))))))

;; takes a collection and converts it into
;; a two-dimensional vector
(defn coll->square [coll]
  (let [size (int (Math/sqrt (count coll)))]
    (mapv vec (partition size coll))))

(defn reference-squares [big-n small-n]
  (let [coords (for [origin (origin-coords big-n small-n)
                     offset (square-offsets small-n)]
                 (mapv + origin offset))]
    (->> (partition (* small-n small-n) coords)
         (map coll->square))))

(defn divisible [x n]
  (when (zero? (mod x n))
    n))

(defn coord? [x]
  (and (vector? x)
       (= (count x) 2)
       (integer? (first x))
       (integer? (second x))))

(defn break-into-squares [matrix n]
  (let [reference-squares (reference-squares (count matrix) n)]
    (map (fn [square]
           (postwalk (fn [form]
                       (if (coord? form)
                         (get-in matrix form)
                         form))
                     square))
         reference-squares)))

;; this one is a real doozy. you have to join the smaller squares
;; back into a big one. I didn't really take that into account so
;; I hacked this join function together. It takes the smaller
;; squares, pairs them with their coordinate of a larger square,
;; then re-maps the element back into the larger square.
(defn join [small-squares]
  (let [big-n (int (Math/sqrt (count (flatten small-squares))))
        small-n (count (first small-squares))
        ref-coords (-> (square-offsets big-n)
                       (coll->square)
                       (break-into-squares small-n))
        ref-coords (->> (flatten ref-coords)
                        (partition 2)
                        (map vec))
        mapped-coords (map (fn [coord e]
                             [coord e])
                           ref-coords
                           (flatten small-squares))]
    (reduce (fn [final-square [coord e]]
              (assoc-in final-square coord e))
            (coll->square (square-offsets big-n))
            mapped-coords)))

(defn grow [matrix rules]
  (let [small-n (or (divisible (count matrix) 2)
                    (divisible (count matrix) 3))
        squares (break-into-squares matrix small-n)
        new-squares (map (fn [square]
                           (get rules square))
                         squares)]
    (join new-squares)))




(comment


  ;; not too proud of the above code...
  (let [enhancement-rules (parse-enhancement-rules input)]
    (->> (last (take (inc 5) (iterate #(grow % enhancement-rules) initial-pattern)))
         (flatten)
         (filter #(= % \#))
         (count)))

  (let [enhancement-rules (parse-enhancement-rules input)]
    (->> (last (take (inc 18) (iterate #(grow % enhancement-rules) initial-pattern)))
         (flatten)
         (filter #(= % \#))
         (count)))



  )