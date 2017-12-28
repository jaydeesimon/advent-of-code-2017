(ns advent-of-code.day17)

(defn shift [curr move size]
  (let [shift* (mod move size)
        [direction shift*] (if (>= (+ curr shift*) size)
                             [-1 (- size shift*)]
                             [1 shift*])]
    (+ curr (* direction shift*))))

(defn insert [v idx val]
  (let [left (subvec v 0 idx)
        right (subvec v idx)]
    (vec (concat left [val] right))))

(defn spin [v curr spin-times n]
  (let [new-curr (inc (shift curr spin-times (count v)))]
    [(insert v new-curr n) new-curr spin-times (inc n)]))

(defn spinnerate [v spin-times]
  (let [f (fn [args]
            (let [[_ curr _ n] args]
              (apply spin args)))]
    (iterate f [v 0 spin-times 1])))

(defn shift* [{:keys [curr move size pos-1]}]
  (let [shift* (mod move size)
        [direction shift*] (if (>= (+ curr shift*) size)
                             [-1 (- size shift*)]
                             [1 shift*])
        next-curr (inc (+ curr (* direction shift*)))]
    {:curr next-curr
     :pos-1 (if (= next-curr 1) size pos-1)
     :move move
     :size (inc size)}))


(comment

  ;; Part 1
  ;; Run this and find the number after 2017
  (last (map first (take (inc 2017) (spinnerate [0] 380))))

  ;; Part 2
  ;; Takes a few mins but returns
  ;; {:curr 43498737, :pos-1 28954211, :move 380, :size 50000001}
  (some (fn [{:keys [curr move size pos-1] :as state}]
          (when (= size (inc 50000000))
            state))
        (iterate shift* {:curr 0 :move 380 :size 1 :pos-1 nil}))

  )