(ns aoc2019.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day6input")
      (slurp)
      (str/split-lines)))

(defn count-orbits
  [data]
  (loop [xs data]))

(defn insert-satellite
  [p s]
  (assoc p s #{}))

(defn build-tree
  [data]
  (reduce
   (fn [acc b]
     (let [x (str/split b #"\)")]
       )
     )
   ()
   data))

(defn build-list
  [data]
  (reduce
   (fn [acc b]
     (let [x (str/split b #"\)")]
       (conj acc {:p (get x 0) :s (get x 1)})))
   ()
   data))

