(ns advent-of-code.day13
  (:require [clojure.string :as str]))

(def input "0: 4\n1: 2\n2: 3\n4: 5\n6: 6\n8: 6\n10: 4\n12: 8\n14: 8\n16: 9\n18: 8\n20: 6\n22: 6\n24: 8\n26: 12\n28: 12\n30: 12\n32: 10\n34: 8\n36: 8\n38: 10\n40: 12\n42: 12\n44: 12\n46: 14\n48: 14\n50: 14\n52: 14\n54: 12\n56: 12\n58: 12\n60: 12\n62: 14\n64: 14\n66: 14\n68: 14\n70: 14\n80: 14\n82: 14\n86: 14\n88: 17\n94: 30\n98: 18")

(def test-input "0: 3\n1: 2\n4: 4\n6: 4")

(defn parse-input [input]
  (into {} (->> (str/split-lines input)
                (map #(str/split % #": "))
                (map (fn [xs]
                       (mapv #(Integer/parseInt %) xs))))))

(defn depth-ranges [input]
  (let [depth-ranges* (parse-input input)]
    (into {} (map #(vector % (get depth-ranges* % 0))
                  (keys depth-ranges*)))))

(defn init-security-scanners [depth-ranges]
  (into {} (map (fn [i]
                  [i [0 1]])
                (keys depth-ranges))))

(defn init-state [depth-ranges]
  {:security-scanners (init-security-scanners depth-ranges)
   :depth-ranges      depth-ranges
   :picosecond        0
   :packet-pos        0})

(defn idx-in-bounds? [size idx]
  (and (>= idx 0) (<= idx (dec size))))

(defn tick-security-scanners [state]
  (reduce (fn [state k]
            (update-in
              state
              [:security-scanners k]
              (fn [[pos direction]]
                (let [max-pos (get-in state [:depth-ranges k])]
                  (if (not (idx-in-bounds? max-pos (+ pos direction)))
                    [(+ pos (* -1 direction)) (* -1 direction)]
                    [(+ pos direction) direction])))))
          state
          (keys (get state :security-scanners))))

(defn tick-packet [state]
  (let [depth-range-keys (keys (get state :depth-ranges))
        last-pos (apply max depth-range-keys)
        packet-pos (get state :packet-pos)]
    (update
      state
      :packet-pos
      (fn [packet-pos]
        (if (not (idx-in-bounds? (inc last-pos) (inc packet-pos)))
          last-pos
          (inc packet-pos))))))

(defn tick-picosecond [state]
  (update state :picosecond inc))

(defn tick [state]
  (-> state
      (tick-security-scanners)
      (tick-packet)
      (tick-picosecond)))

(defn cross-firewall [input]
  (let [depth-ranges (depth-ranges input)
        initial-state (init-state depth-ranges)
        max-layer (apply max (keys depth-ranges))]
    (take (inc max-layer) (iterate tick initial-state))))

(defn find-caught-states [states]
  (filter (fn [{:keys [packet-pos security-scanners]}]
            (let [[security-scanner-pos _] (get security-scanners packet-pos)]
              (and (some? security-scanner-pos) (zero? security-scanner-pos))))
          states))

(defn severity [caught-states]
  (->> (map (fn [{:keys [packet-pos depth-ranges]}]
              (* packet-pos (get depth-ranges packet-pos)))
            caught-states)
       (reduce +)))

(defn whole-trip-severity [input]
  (-> input
      (cross-firewall)
      (find-caught-states)
      (severity)))


(comment

  ;; Part 1
  (whole-trip-severity input)


  )