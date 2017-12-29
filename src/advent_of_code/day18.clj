(ns advent-of-code.day18
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

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

;; Part 1 snd
#_(defmethod instruction-fn "snd" [instruction state]
  (let [[_ x] (str/split instruction #"\s+")
        x (resolve-reg-or-val x (get state :registers))]
    (fn []
      (-> (assoc state :snd x)
          (update :ic inc)))))

;; Part 1 rcv
#_(defmethod instruction-fn "rcv" [instruction state]
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

(defmethod instruction-fn "snd" [instruction state]
  (let [[_ x] (str/split instruction #"\s+")
        x (resolve-reg-or-val x (get state :registers))]
    (fn []
      (-> (update state :snd-queue (fn [queue]
                                     (do
                                       (swap! queue #(conj % x))
                                       queue)))
          (update :snd-cnt inc)
          (update :ic inc)))))

(defmethod instruction-fn "rcv" [instruction state]
  (let [[_ x] (str/split instruction #"\s+")
        x (keyword x)]
    (fn []
      (if (empty? @(get state :rcv-queue))
        state
        (let [rcv-val (peek @(get state :rcv-queue))]
          (-> (assoc-in state [:registers x] rcv-val)
              (update :rcv-queue (fn [queue]
                                   (do
                                     (swap! queue (fn [q] (pop q)))
                                     queue)))
              (update :ic inc)))))))

(def instructions (str/split-lines input))
(def test-instructions (str/split-lines test-input))

(defn initialize-state [pid instructions snd-queue rcv-queue]
  {:instructions instructions
   :ic           0
   :registers    {:p pid}
   :rcv-queue    rcv-queue
   :snd-queue    snd-queue
   :snd-cnt      0})

(defn deadlock? [state0 state1]
  (let [ic0 (get state0 :ic)
        ic1 (get state1 :ic)
        instruction0 (get-in state0 [:instructions ic0])
        instruction1 (get-in state1 [:instructions ic1])]
    (and (str/starts-with? instruction0 "rcv")
         (str/starts-with? instruction1 "rcv")
         (empty? @(get state0 :rcv-queue))
         (empty? @(get state1 :rcv-queue)))))

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

  ;; Part 2
  (let [p0-rcv-queue (atom (PersistentQueue/EMPTY))
        p1-rcv-queue (atom (PersistentQueue/EMPTY))
        p0-state (initialize-state 0 instructions p1-rcv-queue p0-rcv-queue)
        p1-state (initialize-state 1 instructions p0-rcv-queue p1-rcv-queue)]
    (loop [p0-state p0-state
           p1-state p1-state]
      (let [ic0 (get p0-state :ic)
            ic1 (get p1-state :ic)
            instruction0 (get-in p0-state [:instructions ic0])
            instruction1 (get-in p1-state [:instructions ic1])]
        (if (deadlock? p0-state p1-state)
          {:p0 (dissoc p0-state :instructions)
           :p1 (dissoc p1-state :instructions)}
          (recur ((instruction-fn instruction0 p0-state))
                 ((instruction-fn instruction1 p1-state)))))))




  )