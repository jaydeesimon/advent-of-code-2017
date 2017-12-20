(ns advent-of-code.day10
  (:require [clojure.string :as str]))

(def input "34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167")

(def lengths (->> (str/split input #",")
                  (map #(Integer/parseInt %))))

(defn rotate-left [coll n]
  (take (count coll) (drop n (cycle coll))))

(defn rotate-right [coll n]
  (rotate-left coll (- (count coll) n)))


(defn hash-step [coll start end]
  (if (= start end)
    coll
    (let [rotated (rotate-left coll start)
          [section & remainder] (partition-all (- end start) rotated)
          joined (concat (reverse section) (flatten remainder))]
      (rotate-right joined start))))

(defn hash-knot [coll lengths]
  (loop [coll coll
         curr-pos 0
         skip-size 0
         lengths lengths]
    (let [[length & rst] lengths]
      (if (nil? length)
        coll
        (let [curr-pos-normalized (mod curr-pos (count coll))]
          (recur
            (hash-step coll curr-pos-normalized (+ curr-pos-normalized length))
            (+ curr-pos length skip-size)
            (inc skip-size)
            rst))))))

(comment

  ;; Part 1
  (let [[frst scnd] (hash-knot (range 256) lengths)]
    (* frst scnd))

  






  )