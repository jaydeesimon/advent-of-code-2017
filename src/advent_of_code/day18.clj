(ns advent-of-code.day18
  (:require [clojure.string :as str]))

(def input "set i 31\nset a 1\nmul p 17\njgz p p\nmul a 2\nadd i -1\njgz i -2\nadd a -1\nset i 127\nset p 316\nmul p 8505\nmod p a\nmul p 129749\nadd p 12345\nmod p a\nset b p\nmod b 10000\nsnd b\nadd i -1\njgz i -9\njgz a 3\nrcv b\njgz b -1\nset f 0\nset i 126\nrcv a\nrcv b\nset p a\nmul p -1\nadd p b\njgz p 4\nsnd a\nset a b\njgz 1 3\nsnd b\nset f 1\nadd i -1\njgz i -11\nsnd a\njgz f -16\njgz a -19")

(def test-input "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2")

(defmulti instruction-fn (fn [instruction _]
                           (first (str/split instruction #"\s+"))))

(defn register? [x]
  (re-matches #"[a-z]" x))

(defn val? [x]
  (re-matches #"\-?[0-9]+" x))

(defn resolve-reg-or-val [reg-or-val registers]
  (cond
    (register? reg-or-val)
    (get registers (keyword reg-or-val) 0)

    (val? reg-or-val)
    (Integer/parseInt reg-or-val)

    :else
    (throw (ex-info "" {:unknown reg-or-val}))))

(defn normalize-args [instruction registers]
  (let [[_ frst scnd] (str/split instruction #"\s+")]
    [(keyword frst) (resolve-reg-or-val scnd registers)]))

(defmethod instruction-fn "set" [instruction state]
  (let [[reg val] (normalize-args instruction (get state :registers))]
    (fn []
      (-> (assoc-in state [:registers reg] val)
          (update :ic inc)))))

(defn arithmetic-op-fn [op instruction state]
  (let [[reg val] (normalize-args instruction (get state :registers))]
    (fn []
      (-> (update-in state [:registers reg] (fn [x]
                                              (op (or x 0) val)))
          (update :ic inc)))))

(defmethod instruction-fn "mul" [instruction state]
  (arithmetic-op-fn * instruction state))

(defmethod instruction-fn "add" [instruction state]
  (arithmetic-op-fn + instruction state))

(defmethod instruction-fn "mod" [instruction state]
  (arithmetic-op-fn mod instruction state))

(defmethod instruction-fn "jgz" [instruction state]
  (let [[_ x y] (str/split instruction #"\s+")
        [x y] (mapv #(resolve-reg-or-val % (get state :registers)) [x y])]
    (fn []
      (if (pos? x)
        (update state :ic (partial + y))
        (update state :ic inc)))))

(defmethod instruction-fn "snd" [instruction state]
  (let [[_ x] (str/split instruction #"\s+")
        x (resolve-reg-or-val x (get state :registers))]
    (fn []
      (-> (assoc state :snd x)
          (update :ic inc)))))

(defmethod instruction-fn "rcv" [instruction state]
  (let [[_ x] (str/split instruction #"\s+")
        x (resolve-reg-or-val x (get state :registers))]
    (fn []
      (if (not (zero? x))
        (-> (update state :rcv (fn [rcv]
                                 (let [snd (get state :snd)]
                                   (if (nil? rcv)
                                     [snd]
                                     (conj rcv snd)))))
            (update :ic inc))
        (update state :ic inc)))))

(def instructions (str/split-lines input))
(def test-instructions (str/split-lines test-input))

(comment

  ;; Part 1
  (let [initial-state {:instructions instructions
                       :ic 0
                       :registers {}}]
    (loop [state initial-state]
      (let [ic (get state :ic)
            instruction (get-in state [:instructions ic])]
        (if (or (nil? instruction) (get state :rcv))
          (dissoc state :instructions)
          (recur ((instruction-fn instruction state)))))))




  )