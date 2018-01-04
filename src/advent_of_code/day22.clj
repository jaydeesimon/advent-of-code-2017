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

(defn turn-right [direction]
  (let [m {[-1 0] [0 1]
           [0 1]  [1 0]
           [1 0]  [0 -1]
           [0 -1] [-1 0]}]
    (get m direction)))

(def turn-left (comp turn-right turn-right turn-right))
(def turn-around (comp turn-right turn-right))
(def stay-the-course identity)

(defn current-node-state [{:keys [valid-states current] :as world}]
  (let [clean? (comp not (apply some-fn (vals valid-states)))]
    (some (fn [[state state-fn]]
            (when (state-fn current)
              state))
          (concat [[:clean clean?]]
                  valid-states))))

(defn set-node-state [current target-state]
  (fn [valid-states]
    (->> (map (fn [[state nodes]]
                (if (= state target-state)
                  [state (set/union nodes #{current})]
                  [state (set/difference nodes #{current})]))
              valid-states)
         (into {}))))

(defn transition-node-state-fn [node-state-transitions]
  (fn [world current-node-state]
    (let [next-node-state (node-state-transitions current-node-state)
          current (get world :current)]
      (update world :valid-states (set-node-state current next-node-state)))))

(defn transition-direction-fn [direction-transitions]
  (fn [world current-node-state]
    (let [turn-fn (direction-transitions current-node-state)]
      (update world :direction turn-fn))))

(defn advance-current [world]
  (update
    world
    :current
    (fn [current]
      (mapv + current (:direction world)))))

(defn init-world [input]
  {:valid-states {:infected (init-infected input)
                  :weakened #{}
                  :flagged  #{}}
   :direction    [-1 0]
   :current      (->> (str/split-lines input)
                      (first)
                      (count)
                      (center))})

(defn burst-fn [node-state-transitions direction-transitions]
  (let [transition-node-state (transition-node-state-fn node-state-transitions)
        transition-direction (transition-direction-fn direction-transitions)]
    (fn [world]
      (let [current-node-state (current-node-state world)]
        (-> world
            (transition-node-state current-node-state)
            (transition-direction current-node-state)
            (advance-current))))))


(def node-state-transitions-part1 {:infected :clean
                                   :clean    :infected})

(def direction-transitions-part1 {:infected turn-right
                                  :clean    turn-left})

(def node-state-transitions-part2 {:clean :weakened
                                   :weakened :infected
                                   :infected :flagged
                                   :flagged :clean})

(def direction-transitions-part2 {:clean    turn-left
                                  :weakened stay-the-course
                                  :infected turn-right
                                  :flagged  turn-around})


(comment

  ;; Part 1
  (let [world (init-world input)
        burst (burst-fn node-state-transitions-part1
                        direction-transitions-part1)]
    (->> (iterate burst world)
         (take 10000)
         (partition 2 1)
         (map (fn [[{{curr-infected :infected} :valid-states}
                    {{next-infected :infected} :valid-states}]]
                (- (count next-infected) (count curr-infected))))
         (filter pos-int?)
         (reduce +)))

  ;; Part 2
  (let [world (init-world input)
        burst (burst-fn node-state-transitions-part2
                        direction-transitions-part2)]
    (->> (iterate burst world)
         (take 10000000)
         (partition 2 1)
         (map (fn [[{{curr-infected :infected} :valid-states}
                    {{next-infected :infected} :valid-states}]]
                (- (count next-infected) (count curr-infected))))
         (filter pos-int?)
         (reduce +)))

  )