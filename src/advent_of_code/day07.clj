(ns advent-of-code.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split-lines (slurp (io/resource "day-07-input.txt"))))
(def input2 (str/split-lines (slurp (io/resource "day-07-input2.txt"))))

(def test-input (str/split-lines "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"))

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

#_(defn sum [node {:keys [weights adj-list] :as towers}]
  (if-let [children (get adj-list node)
           children-sums (map #(sum % towers) children)]
    ))

(defn sum [node {:keys [weights adj-list] :as towers}]
  (loop [sum (get weights node 0)
         lst (get adj-list node [])]
    (if (empty? lst)
      sum
      (let [[frst & rst] lst]
        (recur
          (+ sum (get weights frst 0))
          (concat rst (get adj-list frst [])))))))

(defn balances [{:keys [weights adj-list] :as towers}]
  (map (fn [[node node-child]]
            (vector node node-child (sum node-child towers)))
          (for [node (keys weights)
                node-child (get adj-list node [])]
            [node node-child])))


(comment

  ;; part one
  (find-root (parse-input input))

  (def towers (parse-input input))
  (def test-towers (parse-input test-input))
  (def towers2 (parse-input input2))


  (let [towers (parse-input input)
        root (find-root towers)]
    (sum root towers))

  (sum "urbkrn" (parse-input input))

  (->> (balances (parse-input input))
       (group-by first)
       (vals)
       (filter (fn [tuples]
                 (apply not= (map #(nth % 2) tuples)))))

  (->> (balances (parse-input (str/split-lines (slurp (io/resource "day-07-input2.txt")))))
       (group-by first)
       (vals)
       (filter (fn [tuples]
                 (apply not= (map #(nth % 2) tuples)))))

  (->> (balances (parse-input test-input))
       (group-by first)
       (vals)
       (filter (fn [tuples]
                 (apply not= (map #(nth % 2) tuples)))))

  (sum "ztjquwi" (parse-input input))


  )