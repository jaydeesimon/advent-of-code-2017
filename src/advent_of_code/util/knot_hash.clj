(ns advent-of-code.util.knot-hash)

;; The names are bad but I copied this straight from Day 10

(defn- str->ascii [s]
  (vec (.getBytes s "US-ASCII")))

(defn- str->lengths [s]
  (vec (concat (str->ascii s) [17 31 73 47 23])))

(defn- rotate-left [coll n]
  (take (count coll) (drop n (cycle coll))))

(defn- rotate-right [coll n]
  (rotate-left coll (- (count coll) n)))

(defn- hash-step [coll start end]
  (if (= start end)
    coll
    (let [rotated (rotate-left coll start)
          [section & remainder] (partition-all (- end start) rotated)
          joined (concat (reverse section) (flatten remainder))]
      (rotate-right joined start))))

(defn- knot-hash-part2 [{:keys [coll curr-pos skip-size]} lengths]
  (loop [coll coll
         curr-pos curr-pos
         skip-size skip-size
         lengths lengths]
    (let [[length & rst] lengths]
      (if (nil? length)
        {:coll coll
         :curr-pos curr-pos
         :skip-size skip-size}
        (let [curr-pos-normalized (mod curr-pos (count coll))]
          (recur
            (hash-step coll curr-pos-normalized (+ curr-pos-normalized length))
            (+ curr-pos length skip-size)
            (inc skip-size)
            rst))))))

(defn- part-two [coll lengths n]
  (let [x {:coll coll :curr-pos 0 :skip-size 0}]
    (take (inc n) (iterate #(knot-hash-part2 % lengths) x))))

(defn knot-hash [input]
  (let [sparse-hash (:coll (last (part-two (range 256) (str->lengths input) 64)))]
    (->> (partition 16 sparse-hash)
         (map #(apply bit-xor %))
         (map #(format "%02x" %))
         (apply str))))


