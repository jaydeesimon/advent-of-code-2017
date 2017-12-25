(ns advent-of-code.day15)

(defn make-generator [initial factor]
  (let [prev (atom initial)]
    (fn []
      (let [v (mod (* @prev factor) 2147483647)]
        (do
          (reset! prev v)
          v)))))

(defn make-second-generator [generator factor]
  (fn []
    (loop [n (generator)]
      (if (zero? (mod n factor))
        n
        (recur (generator))))))

(def a-gen (make-generator 618 16807))
(def b-gen (make-generator 814 48271))

(def a-gen2 (make-second-generator a-gen 4))
(def b-gen2 (make-second-generator b-gen 8))

(def a (fn [] (bit-and (a-gen) 0x0000FFFF)))
(def b (fn [] (bit-and (b-gen) 0x0000FFFF)))

(def a2 (fn [] (bit-and (a-gen2) 0x0000FFFF)))
(def b2 (fn [] (bit-and (b-gen2) 0x0000FFFF)))

(comment

  ;; Part 1
  (filter (fn [[a b]]
            (= a b))
          (take 40000000 (repeatedly (juxt a b))))

  ;; Part 2
  (filter (fn [[a b]]
            (= a b))
          (take 5000000 (repeatedly (juxt a2 b2))))

  )