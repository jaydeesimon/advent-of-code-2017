(ns advent-of-code.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split-lines (slurp (io/resource "day-07-input.txt"))))

(defn parse-line [line]
  (let [re (if (str/includes? line "->")
             #"^(\S+) \((\d+)\) -> (.*)$"
             #"^(\S+) \((\d+)\)$")
        [[_ tower weight children]] (re-seq re line)
        weight (Integer/parseInt weight)
        children (when children
                   (mapv str/trim (str/split children #",")))]
    {:tower tower
     :weight weight
     :children children}))

(defn parse-input [input]
  (reduce (fn [towers line]
            (let [{:keys [tower weight children]} (parse-line line)]
              (if (seq children)
                (-> (assoc-in towers [:weights tower] weight)
                    (assoc-in [:adj-list tower] children))
                (assoc-in towers [:weights tower] weight))))
          {:weights {}
           :adj-list {}}
          input))

(defn find-root [{:keys [adj-list]}]
  (let [children (set (flatten (vals adj-list)))
        towers (set (keys adj-list))
        root (set/difference towers children)]
    (when (= (count root) 1)
      (first root))))


(comment

  ;; part one
  (find-root (parse-input input))

  )