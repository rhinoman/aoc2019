(ns aoc2019.day1
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))


(def input
  "Loads the puzzle input into a nice vector"
  (->> (io/resource "day1input")
       (slurp)
       (split-lines)
       (map #(Integer/parseInt %))
       (into [])))

(defn fuel-req
  "Computes the fuel requirement for a single module"
  [^Integer mm]
  (-> mm
      (/ 3)
      (Math/floor)
      (- 2)
      (int)))

(defn sum-fuel-req
  "Solution to Part 1.  Computes the sum of all module fuel requirements"
  []
  (reduce (fn [acc b] (+ acc (fuel-req b))) 0 input))

(defn fuel-req-2
  "Computes fuel requirement for a module. Including the mass of the fuel"
  [^Integer mm]
  (loop [fr (fuel-req mm)
         sum 0]
    (if-not (> fr 0)
      sum
      (recur (fuel-req fr) (+ sum fr)))))

(defn sum-fuel-req-2
  "Solution to Part 2.  Computes all fuel requirements, including fuel"
  []
  (reduce (fn [acc b] (+ acc (fuel-req-2 b))) 0 input))
