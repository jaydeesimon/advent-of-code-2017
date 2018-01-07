(ns advent-of-code.experimental.day24
  (:require [clojure.math.combinatorics :refer [count-subsets subsets]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def test-input "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10")

(def input "32/31\n2/2\n0/43\n45/15\n33/24\n20/20\n14/42\n2/35\n50/27\n2/17\n5/45\n3/14\n26/1\n33/38\n29/6\n50/32\n9/48\n36/34\n33/50\n37/35\n12/12\n26/13\n19/4\n5/5\n14/46\n17/29\n45/43\n5/0\n18/18\n41/22\n50/3\n4/4\n17/1\n40/7\n19/0\n33/7\n22/48\n9/14\n50/43\n26/29\n19/33\n46/31\n3/16\n29/46\n16/0\n34/17\n31/7\n5/27\n7/4\n49/49\n14/21\n50/9\n14/44\n29/29\n13/38\n31/11")


(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-component [line]
  (mapv parse-int (str/split line #"/")))

(defn find-candidates [connection components]
  (filter (fn [[front back]]
            ((set [front back]) connection))
          components))

(defn attach-component [component chain]
  (let [[front back] component]
    (if (= front (last chain))
      (concat chain [front back])
      (concat chain [back front]))))

(defn chain-components-random [initial components]
  (loop [chain initial
         components (set components)]
    (let [candidates (find-candidates (last chain) components)]
      (if (not (seq candidates))
        chain
        (let [candidate (rand-nth candidates)]
          (recur
            (attach-component candidate chain)
            (set/difference components #{candidate})))))))

(defn search-strongest-chain [components n]
  (->> (repeatedly #(chain-components-random [0] components))
       (take n)
       (map (fn [chain]
              [(reduce + chain) chain]))
       (reduce (partial max-key first))))


(comment

  ;; Tried this thing where I randomly generate chains based on the
  ;; rules and tried to see if I could stumble on the answer.
  ;; I tried 1 million iterations and 10 million iterations
  ;; and they both arrived at the same answer 1503, neither of
  ;; which worked (too low). Interestingly, they are slightly different
  ;; chains. Giving up on this approach but leaving it here
  ;; for reference.

  ;; 1 million iterations = [0 16 16 3 3 50 50 32 32 31 31 46 46 29 29 29 29 17 17 1 1 26 26 13 13 38 38 33 33 19 19 4 4 7 7 33 33 50 50 43 43 45 45 5 5 5 5 27 27 50 50 9 9 48 48 22 22 41] = 1503

  ;; 10 million iterations = [0 16 16 3 3 50 50 43 43 45 45 5 5 5 5 27 27 50 50 33 33 7 7 4 4 19 19 33 33 38 38 13 13 26 26 1 1 17 17 29 29 29 29 46 46 31 31 32 32 50 50 9 9 48 48 22 22 41] = 1503
  (let [components (map parse-component (str/split-lines input))]
    (time (search-strongest-chain components 100)))

  )
