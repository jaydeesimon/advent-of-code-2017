(ns advent-of-code.day22
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input "#..###...#..#.#.#...#..##\n....#.##..#..###...###...\n##.#.....###...#.##...###\n##...#.####..#####..####.\n##.#...#.##...##.....##.#\n###.#.#...###..###.###...\n#.#..#.#.###..#.##.#..###\n.#..###.##..##.#....#.#..\n#.#.......###.##...#.##..\n#.#.######.##.#..#...#...\n######.#.##...#.#...###.#\n.#....#.###.##.######....\n#.#####.#####.#.#..##.###\n..##.#.#...###......###.#\n.##.##..##.#.#.#######.##\n#..###.###....#.....##..#\n..##..####..##.#...####..\n.##.####.##.##..##..#....\n###...#.#..##...#.#..##..\n......##.....#.#..#.#.###\n#.#.##.##.#####....#.#..#\n.....#.###.##...#...#..#.\n#...#......##.##.#####.##\n#.##.##.......#.##....#.#\n####.##.#.#........###.##")

(def test-input "..#\n#..\n...")

(defn coords [n]
  (for [x (range n) y (range n)] [x y]))

(defn sqrt [n]
  (int (Math/sqrt n)))

(defn center [n]
  (let [coords (coords n)]
    (mapv (fn [x y]
            (/ (+ x y) 2))
          (first coords)
          (last coords))))

(defn init-infected [input]
  (let [flat-nodes (str/replace input #"\n" "")]
    (->> (map (fn [node coord]
                [node coord])
              flat-nodes
              (coords (sqrt (count flat-nodes))))
         (filter (fn [[node _]]
                   (= node \#)))
         (map (fn [[_ coord]]
                coord))
         (set))))

(defn init-state [input]
  {:infected  (init-infected input)
   :direction [-1 0]
   :current   (->> (str/split-lines input)
                   (first)
                   (count)
                   (center))})

(defn turn-left [direction]
  (let [m {[-1 0] [0 -1]
           [0 -1] [1 0]
           [1 0]  [0 1]
           [0 1]  [-1 0]}]
    (get m direction)))

(defn turn-right [direction]
  (let [m {[-1 0] [0 1]
           [0 1] [1 0]
           [1 0] [0 -1]
           [0 -1] [-1 0]}]
    (get m direction)))

(defn flip-node [infected node]
  (if (infected node)
    (set/difference infected #{node})
    (set/union infected #{node})))

(defn burst [{:keys [infected direction current] :as state}]
  (let [turn-fn (if (infected current) turn-right turn-left)
        new-direction (turn-fn direction)]
    (-> (assoc state :direction new-direction)
        (update :infected #(flip-node % current))
        (update :current #(mapv + % new-direction)))))

(comment

  ;; Part 1
  (let [state (init-state input)]
    (->> (take 10000 (iterate burst state))
         (partition 2 1)
         (map (fn [[{curr-infected :infected} {next-infected :infected}]]
                (- (count next-infected) (count curr-infected))))
         (filter pos-int?)
         (reduce +)))















  )