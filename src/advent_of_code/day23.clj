(ns advent-of-code.day23
  (:require [clojure.string :as str]))

(def input "set b 93\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23")

(defn try-parse-int [x]
  (try (Integer/parseInt x) (catch Exception _ nil)))

(defn parse-arg [x]
  (or (try-parse-int x) (keyword x)))

(defn parse-line [line]
  (let [[instr & args] (str/split line #" ")]
    {:instruction (keyword instr)
     :args (mapv parse-arg args)}))

(defn init-runtime [input]
  {:program (mapv parse-line (str/split-lines input))
   :ic 0
   :registers {}})

(defn resolve' [registers reg-or-val]
  (cond
    (keyword? reg-or-val) (get registers reg-or-val 0)
    (integer? reg-or-val) reg-or-val
    :else (throw (ex-info "" {:unknown reg-or-val}))))

(defmulti execute (fn [instruction _]
                    (:instruction instruction)))

(defmethod execute :set [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers arg1] (resolve' registers arg2))
      (update-in [:ic] inc)))

(defmethod execute :mul [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (-> state
      (update-in [:registers arg1] #(* (or % 0) (resolve' registers arg2)))
      (update-in [:ic] inc)))

(defmethod execute :sub [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (-> state
      (update-in [:registers arg1] #(- (or % 0) (resolve' registers arg2)))
      (update-in [:ic] inc)))

(defmethod execute :jnz [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (let [x (resolve' registers arg1)
        y (resolve' registers arg2)]
    (let [update-ic (if (zero? x) inc #(+ % y))]
      (update-in state [:ic] update-ic))))

(defn count-instruction [instruction state]
  (update-in state [:counts instruction] (fnil inc 0)))

(defn execute-program [input]
  (let [{:keys [program ic registers]} (init-runtime input)]
    (loop [state {:ic ic
                  :registers registers
                  :counts {}}]
      (let [instruction (get program (:ic state) :eop)]
        (if (= instruction :eop)
          state
          (recur (->> (execute instruction state)
                      (count-instruction (:instruction instruction)))))))))

(comment

  ;; Part 1
  (execute-program input)




  )
