(ns advent-of-code.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(defn parse-line [line]
  (let [[[_ l r]] (re-seq #"(\d+) <-> (.*)$" line)
        rs (str/split r #", ")]
    [l rs]))

(defn parsed-line->edges [[l r]]
  (for [left [l]
        right r]
    [left right]))

(defn input->edges [input]
  (->> (str/split-lines input)
       (map parse-line)
       (mapcat parsed-line->edges)
       (map (fn [[l r]]
              [l (set [r])]))
       (reduce (fn [edges [k v]]
                 (merge-with set/union edges {k v}))
               {})))

(def edges (input->edges (slurp (io/resource "day-12-input.txt"))))

(defn traverse-pipes [edges start]
  (loop [q (conj PersistentQueue/EMPTY start)
         visited? #{}
         path []]
    (if (empty? q)
      path
      (let [curr (peek q)
            neighbors (get edges curr #{})
            visited? (set/union visited? #{curr})
            never-seen (set/difference neighbors (set/union (set q) visited?))]
        (recur (apply conj (pop q) never-seen)
               visited?
               (conj path curr))))))

(def test-input "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")

(comment

  (traverse-pipes (input->edges test-input) "0")

  ;; Part 1
  (count (traverse-pipes edges "0"))

  )