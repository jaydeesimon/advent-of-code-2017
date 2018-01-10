(ns advent-of-code.day24
  (:require [clojure.set :as set]
            [clojure.string :as str]))

#_(def test-components [[0 2]
                        [2 2]
                        [2 3]
                        [3 4]
                        [3 5]
                        [0 1]
                        [10 1]
                        [9 10]])

(def input "32/31\n2/2\n0/43\n45/15\n33/24\n20/20\n14/42\n2/35\n50/27\n2/17\n5/45\n3/14\n26/1\n33/38\n29/6\n50/32\n9/48\n36/34\n33/50\n37/35\n12/12\n26/13\n19/4\n5/5\n14/46\n17/29\n45/43\n5/0\n18/18\n41/22\n50/3\n4/4\n17/1\n40/7\n19/0\n33/7\n22/48\n9/14\n50/43\n26/29\n19/33\n46/31\n3/16\n29/46\n16/0\n34/17\n31/7\n5/27\n7/4\n49/49\n14/21\n50/9\n14/44\n29/29\n13/38\n31/11")

(def components (->> (str/split-lines input)
                     (map #(str/split % #"/"))
                     (map (fn [t] (mapv #(Integer/parseInt %) t)))))

(defn find-components-with-connection [connection components]
  (filter #((set %) connection) components))

(defn attach-component [bridge component]
  (let [[front back] component]
    (if (= front (last bridge))
      (concat bridge [front back])
      (concat bridge [back front]))))

(defn bridges* [bridge components]
  (let [components-to-attach (find-components-with-connection (last bridge) components)]
    (if (not (seq components-to-attach))
      (concat bridge [\*])
      (map (fn [component-to-attach]
             (bridges*
               (attach-component bridge component-to-attach)
               (set/difference components #{component-to-attach})))
           components-to-attach))))

(defn bridges [components]
  (->> (bridges* [0] (set components))
       (flatten)
       (partition-by #(= % \*))
       (remove #(= % '(\*)))))

(comment

  ;; Part 1
  (reduce max (map (partial reduce +) (bridges components)))

  ;; Part 2
  (->> (bridges components)
       (sort-by (juxt count (partial reduce +)) (fn [[c1 s1] [c2 s2]]
                                                  (if (= c1 c2)
                                                    (> s1 s2)
                                                    (> c1 c2))))
       (first)
       (reduce +))

  )

