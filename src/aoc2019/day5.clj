(ns aoc2019.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day5input")
      (slurp)
      (str/split #",")))

(defn parse-op
  [operation]
  (let [len (count operation)
        op (Character/digit (or (get operation (dec len)) \9) 10)
        mode1 (Character/digit (or (get operation (- len 3)) \0) 10)
        mode2 (Character/digit (or (get operation (- len 4)) \0) 10)
        mode3 (Character/digit (or (get operation (- len 5)) \0) 10)]
    {:opcode op :mode1 mode1 :mode2 mode2 :mode3 mode3}))

(defn ex-add [op ptr mem]
  (let [x (get mem (+ ptr 1))
        y (get mem (+ ptr 2))
        ap (get mem (+ ptr 3))
        xv (if (= :mode1 1) x (get mem x))
        yv (if (= :mode2 1) y (get mem y))]
    (assoc mem ap (+ xv yv))))

(defn ex-mul [op ptr mem]
  (let [x (get mem (+ ptr 1))
        y (get mem (+ ptr 2))
        ap (get mem (+ ptr 3))
        xv (if (= :mode1 1) x (get mem x))
        yv (if (= :mode2 1) y (get mem y))]
    (assoc mem ap (* xv yv))))

(defn ex-sto [op ptr mem])

(defn ex-out [op ptr mem])

(defn eval-op [op ptr mem]
  (cond
    (= (:opcode op) 1) {:result (ex-add op ptr mem) :ptr (+ ptr 4)}
    (= (:opcode op) 2) {:result (ex-mul op ptr mem) :ptr (+ ptr 4)}
    (= (:opcode op) 3) {:result (ex-sto op ptr mem) :ptr (+ ptr 2)}
    (= (:opcode op) 4) {:result (ex-out op ptr mem) :ptr (+ ptr 2)}))

(defn read-program [prog]
  (loop [ptr 0
         mem prog]
    (let [this-op (parse-op (get mem ptr))]
      (if (= (:opcode this-op) 9)
        mem)
      (let [result (eval-op this-op ptr mem)]
        (recur (:ptr result) (:result result))))))





