(ns advent-of-code.day23
  (:require [clojure.string :as str]))

(def input "set b 93\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nset e b\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23\n")
#_(def test-input "set b 93\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23")

(defn try-parse-int [x]
  (try (Integer/parseInt x) (catch Exception _ nil)))

(defn parse-arg [x]
  (or (try-parse-int x) (keyword x)))

(defn parse-line [line]
  (when (not (str/starts-with? line "#"))
    (let [[instr & args] (str/split line #" ")]
      {:instruction (keyword instr)
       :args        (mapv parse-arg args)})))

(defn init-runtime
  ([input]
   (init-runtime input 0))
  ([input ic]
   {:program   (filterv some? (map parse-line (str/split-lines input)))
    :ic        ic
    :registers {}}))

(defn resolve' [registers reg-or-val]
  (cond
    (keyword? reg-or-val) (get registers reg-or-val 0)
    (integer? reg-or-val) reg-or-val
    :else (throw (ex-info "" {:unknown reg-or-val}))))

(defmulti execute (fn [instruction _]
                    (:instruction instruction)))

(defn arithmetic-instruction [f {[dest val] :args} {:keys [registers] :as state}]
  (-> state
      (update-in [:registers dest] #(f (or % 0) (resolve' registers val)))
      (update-in [:ic] inc)))

(defmethod execute :mul [instruction state]
  (arithmetic-instruction * instruction state))

(defmethod execute :sub [instruction state]
  (arithmetic-instruction - instruction state))

(defmethod execute :set [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (-> state
      (assoc-in [:registers arg1] (resolve' registers arg2))
      (update-in [:ic] inc)))

(defmethod execute :jnz [{[arg1 arg2] :args} {:keys [registers] :as state}]
  (let [x (resolve' registers arg1)
        y (resolve' registers arg2)]
    (let [update-ic (if (zero? x) inc #(+ % y))]
      (update-in state [:ic] update-ic))))

(defn update-debug-info [thing debug-info]
  (-> (update-in debug-info [:counts thing] (fnil inc 0))))

(defn execute-program
  ([input] (execute-program input 0 {}))
  ([input ic initial-registers]
   (let [{:keys [program ic registers]} (init-runtime input ic)
         registers (merge registers initial-registers)]
     (loop [state {:ic        ic
                   :registers registers}
            counter 0
            debug-info {:counts {}}]
       (let [instruction (get program (:ic state) :eop)
             _ (println (inc (:ic state)) instruction (:registers state))]
         (if (= instruction :eop)
           {:state state
            :debug-info debug-info}
           (let [new-state (execute instruction state)]
             (recur new-state
                    (inc counter)
                    (update-debug-info (:instruction instruction) debug-info)))))))))

(comment

  ;; Part 1
  (execute-program input)

  #_(execute-program input 0 {:a 1})

  ;; I'm not sure how the "interpreter" written in part 1 can be used
  ;; to solve part 2. The answer was arrived by reverse-engineering the
  ;; instructions, figuring out what it's actually doing and then
  ;; write a program to do the same thing but more efficiently. In the
  ;; case of part 2, it's counting the number of non-primes between 2 up to b.
  ;; Thanks @capablemonkey for your help!


  )
