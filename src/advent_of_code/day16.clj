(ns advent-of-code.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn spin [coll n]
  (vec (take (count coll) (drop (- (count coll) n) (cycle coll)))))

(defn exchange [coll a b]
  (let [a* (get coll a)
        b* (get coll b)]
    (-> coll
        (assoc a b*)
        (assoc b a*))))

(defn find-elem [coll a]
  (->> (map vector coll (range))
       (some (fn [[elem idx]]
               (when (= a elem)
                 idx)))))

(defn partner [coll a b]
  (let [i (find-elem coll a)
        j (find-elem coll b)]
    (exchange coll i j)))

(defn spin-fn [instruction]
  (let [re #"s(\d+)"
        [[_ n]] (re-seq re instruction)
        n (Integer/parseInt n)]
    (fn [coll]
      (spin coll n))))

(defn exchange-fn [instruction]
  (let [re #"x(\d+)/(\d+)"
        [[_ a b]] (re-seq #"x(\d+)/(\d+)" instruction)
        a (Integer/parseInt a)
        b (Integer/parseInt b)]
    (fn [coll]
      (exchange coll a b))))

(defn partner-fn [instruction]
  (let [re #"p(.)/(.)"
        [[_ a b]] (re-seq re instruction)
        [a] a
        [b] b]
    (fn [coll]
      (partner coll a b))))

(defn instruction-fn [instruction]
  (case (first instruction)
    \p (partner-fn instruction)
    \x (exchange-fn instruction)
    \s (spin-fn instruction)
    (throw (ex-info "" {:unknown instruction}))))

(def dance-input (str/split (slurp (io/resource "day-16-input.txt")) #","))
(def test-dance-input (str/split "s1,x3/4,pe/b" #","))

(defn dance-fn [dance-input]
  (reduce (fn [curr-f instruction]
            (comp curr-f (instruction-fn instruction)))
          identity
          (reverse dance-input)))

(comment

  ;; Part 1
  (apply str
         (reduce (fn [program instruction]
                   ((instruction-fn instruction) program))
                 (vec "abcdefghijklmnop")
                 dance-input))

  ;; This is way too slow
  (time
    (let [dance-fn (dance-fn dance-input)
          iterations 1000000000]
      (last (take iterations (iterate dance-fn (vec "nlciboghjmfdapek"))))))

  ;; But it repeats every 63 iterations
  (let [dance-fn (dance-fn dance-input)]
    (->> (map (fn [idx position]
                [idx position])
              (range)
              (iterate dance-fn (vec "nlciboghjmfdapek")))
         (filter (fn [[idx position]]
                   (= position (vec "nlciboghjmfdapek"))))
         (take 5)))

  ;; So we can do
  (mod 1000000000 63) ;; => 55


  ;; Part 2
  ;; Since it's zero-based, it'll actually be the 54th position
  (let [dance-fn (dance-fn dance-input)]
    (->> (map (fn [idx position]
                [(mod idx 63) (apply str position)])
              (range)
              (iterate dance-fn (vec "nlciboghjmfdapek")))
         (take 128)))

  )