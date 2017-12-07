(ns advent-of-code.day06
  (:require [clojure.string :as str]))

(def input "4\t1\t15\t12\t0\t9\t9\t5\t5\t8\t7\t3\t14\t5\t12\t3")

(def memory-banks (->> (str/split input #"\t")
                       (mapv #(Integer/parseInt %))))

(defn distribute-cycle* [memory-banks pos]
  (let [blocks (get memory-banks pos)
        memory-banks (assoc memory-banks pos 0)]
    (loop [blocks blocks
           memory-banks memory-banks
           pos (mod (inc pos) (count memory-banks))]
      (if (zero? blocks)
        memory-banks
        (recur
          (dec blocks)
          (update memory-banks pos inc)
          (mod (inc pos) (count memory-banks)))))))

(defn most-blocks [memory-banks]
  (->> (map vector (range) memory-banks)
       (sort-by (juxt second first) #(and (> (first %1) (first %2))
                                          (> (second %1) (second %2))))
       (ffirst)))

(defn distribute-cycle [memory-banks choose-memory-bank-fn]
  (distribute-cycle*
    memory-banks
    (choose-memory-bank-fn memory-banks)))

(defn cycles-until-repeat [memory-banks]
  (loop [memory-banks memory-banks
         memory-banks-vec [memory-banks]]
    (let [memory-banks (distribute-cycle memory-banks most-blocks)]
      (if ((set memory-banks-vec) memory-banks)
        (conj memory-banks-vec memory-banks)
        (recur
          memory-banks
          (conj memory-banks-vec memory-banks))))))

(defn distribute-cycles [memory-banks]
  (iterate #(distribute-cycle % most-blocks) memory-banks))

(comment

  (distribute-cycle [1 3 4 1] most-blocks)

  (take 10 (distribute-cycles memory-banks))

  ;; part one
  (count (cycles-until-repeat memory-banks))

  ;; repeat memory bank
  ;; got this by running cycles-until-repeat and
  ;; looked at the last element
  [0 14 13 12 11 10 8 8 6 6 5 3 3 2 1 10]

  ;; part two
  (->> (cycles-until-repeat memory-banks)
       (map vector (range))
       (filter (fn [[idx memory-bank]]
                 (= memory-bank [0 14 13 12 11 10 8 8 6 6 5 3 3 2 1 10])))
       (map (fn [[idx _]]
              idx))
       (reverse)
       (apply -))



  )