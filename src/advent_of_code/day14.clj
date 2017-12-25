(ns advent-of-code.day14
  (:require [advent-of-code.util.knot-hash :refer [knot-hash]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn hex-digit->binary [hd]
  (let [bs (-> (Integer/valueOf (str hd) 16)
               (Integer/toBinaryString))
        pad-n (- 4 (count bs))]
    (str (apply str (repeat pad-n "0")) bs)))

(defn hex->binary [hs]
  (str/join (map hex-digit->binary hs)))

(defn binary->grid [b]
  (apply str (map (fn [d]
                    (if (= \1 d) \# \.))
                  b)))

(defn generate-grid [s]
  (map (fn [n]
         (let [s (str s "-" n)]
           (-> (knot-hash s)
               (hex->binary)
               (binary->grid))))
       (range 128)))

(defn neighbors [grid on]
  (fn [coord]
    (->> (map #(mapv + coord %) [[0 1] [1 0] [-1 0] [0 -1]])
         (filter #(= on (get-in grid %))))))

(defn explore-region [grid coord]
  (let [neighbors-fn (neighbors grid \#)]
    (loop [frontier [coord]
           visited #{coord}]
      (let [[coord & frontier] frontier]
        (if (nil? coord)
          visited
          (recur (into frontier (->> (neighbors-fn coord)
                                     (remove visited)))
                 (into visited [coord])))))))

(defn grid-coords [grid]
  (for [x (range (count grid))
        y (range (count (first grid)))]
    [x y]))

(defn on-coords [grid on]
  (filter #(= on (get-in grid %)) (grid-coords grid)))

(defn find-all-regions [grid]
  (loop [coords-unexplored (set (on-coords grid \#))
         regions []]
    (if (empty? coords-unexplored)
      regions
      (let [region (explore-region grid (first coords-unexplored))]
        (recur (set/difference coords-unexplored (set region))
               (conj regions region))))))


(comment

  ;; Part 1
  (->> (generate-grid "wenycdww")
       (map (fn [row]
              (count (filter #(= % \#) row))))
       (reduce +))

  ;; Part 2
  (->> (generate-grid "wenycdww")
       (mapv vec)
       (find-all-regions)
       (count))


  )