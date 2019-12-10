(ns aoc2019.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day10input")
       (slurp)
       (asteroid-locations)))


(def tp-1
  (-> ".#..#
.....
#####
....#
...##"
  (asteroid-locations)))

(defn asteroid-locations [data]
  (let [rows (str/split-lines data)]
    (loop [row (first rows)
           rst (rest rows)
           row-num 0
           coords []]
      (if (nil? row)
        coords
        (let [these-coords (keep-indexed (fn [idx itm] (if (= itm \#) [idx row-num] nil)) row)]
          (recur (first rst) (rest rst) (inc row-num) (concat these-coords coords)))))))
