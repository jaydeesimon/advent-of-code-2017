(ns advent-of-code.day14
  (:require [advent-of-code.util.knot-hash :refer [knot-hash]]
            [clojure.string :as str]))

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


(comment

  ;; Part 1
  (->> (generate-grid "wenycdww")
       (map (fn [row]
              (count (filter #(= % \#) row))))
       (reduce +))

  )