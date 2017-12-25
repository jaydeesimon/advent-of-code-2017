(ns advent-of-code.day15)

(defn make-generator [initial factor]
  (let [prev (atom initial)]
    (fn []
      (let [v (mod (* @prev factor) 2147483647)]
        (do
          (reset! prev v)
          v)))))

(def a-gen (make-generator 618 16807))
(def b-gen (make-generator 814 48271))

(def a (fn [] (bit-and (a-gen) 0x0000FFFF)))
(def b (fn [] (bit-and (b-gen) 0x0000FFFF)))

(comment

  (filter (fn [[a b]]
            (= a b))
          (take 40000000 (repeatedly (juxt a b))))


  )