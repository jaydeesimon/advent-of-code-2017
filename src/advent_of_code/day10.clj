(ns advent-of-code.day10)


(defn rotate-left [coll n]
  (take (count coll) (drop n (cycle coll))))

(defn rotate-right [coll n]
  (rotate-left coll (- (count coll) n)))

(defn hash-step [coll start end]
  (let [rotated (rotate-left coll start)
        [section & remainder] (partition-all (- end start) rotated)
        joined (concat (reverse section) (flatten remainder))]
    (rotate-right joined start)))

(comment

  



  )