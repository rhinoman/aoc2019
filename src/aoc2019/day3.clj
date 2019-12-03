(ns aoc2019.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day3input")
      (slurp)
      (str/split-lines)))

(def wire1
  (-> (get input 0)
      (str/split #",")))

(def wire2
  (-> (get input 1)
      (str/split #",")))

(defn distance-from-port [x y]
  (+ x y))

(defn dv [cur nx]
  (let [direction (get nx 0)
        mag (Integer/parseInt (subs nx 1))]
    [direction, mag]))

(defn wire-coords
  "Converts wire sequence into a list of coords"
  [wire]
  (reduce
   (fn [acc b]
     (let [prev (first acc)
           next (first wire1)]))
   [[0 0]]
   wire1))
