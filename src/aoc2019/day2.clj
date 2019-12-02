(ns aoc2019.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [permutations
                                                combinations
                                                count-combinations
                                                selections]]))

(def test-input-1 [1 9 10 3 2 3 11 0 99 30 40 50])

(def p2r 19690720)

(def input
  (into [] (map
            #(Integer/parseInt %)
            (-> (io/resource "day2input")
                (slurp)
                (str/split #",")))))

(defn calc-result
  [op x y]
  (cond
    (= op 1) (+ x y)
    (= op 2) (* x y)
    :else 0))

(defn eval-op
  "Evaluates a single operation. Returns the modified input"
  [operation xs]
  (let [[op xp yp ap] operation
        x (get xs xp)
        y (get xs yp)]
    (assoc xs ap (calc-result op x y))))

(defn run-program
  "Runs a program"
  [prog]
  (loop [operations (partition-all 4 prog)
         result prog]
    (let [this-op (first operations)]
      (if (= 99 (first this-op))
        result
        (recur (rest operations)
               (eval-op this-op result))))))

(defn solve-part-1
  []
  (let [fixed-input (-> input (assoc 1 12) (assoc 2 2))
        result (run-program fixed-input)]
    (first result)))

(defn run-part-2
  []
  (let [inputs (selections (range 0 100) 2)]
    (loop [remaining-inputs inputs]
      (let [params (into [] (first remaining-inputs))
            adjusted (-> input
                         (assoc 1 (get params 0))
                         (assoc 2 (get params 1)))]
        (cond 
          (< (count params) 2) [-99]
          (= (first (run-program adjusted)) p2r) params
          :else (recur (rest remaining-inputs)))))))


(defn solve-part-2
  []
  (let [result (run-part-2)
        noun (get result 0)
        verb (get result 1)]
    (+ (* 100 noun) verb)))
