(ns aoc2019.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [intersection]]))

(def input
  (-> (io/resource "day3input")
      (slurp)
      (str/split-lines)))

(defn dv [cur nx]
  (let [x (get cur 0)
        y (get cur 1)
        direction (subs nx 0 1)
        mag (Integer/parseInt (subs nx 1))
        is (range 1 (+ 1 mag))]
    (cond
        (= direction "U") (map (fn [i] [x (+ y i)]) is) 
        (= direction "D") (map (fn [i] [x (- y i)]) is)
        (= direction "L") (map (fn [i] [(- x i) y]) is)
        (= direction "R") (map (fn [i] [(+ x i) y]) is)
        :else cur)))

(defn wire-coords
  "Converts wire sequence into a list of coords"
  [wire]
  (reduce
   (fn [acc b]
     (let [prev (last acc)
           next (dv prev b)]
       (concat acc next)))
   [[0 0]]
   wire))

(def wire1
  (-> (get input 0)
      (str/split #",")
      (wire-coords)))

(def wire2
  (-> (get input 1)
      (str/split #",")
      (wire-coords)))

(def test-wire1
  (-> "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
      (str/split #",")
      (wire-coords)))

(def test-wire2
  (-> "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      (str/split #",")
      (wire-coords)))

(def test-wire3
  (-> "R75,D30,R83,U83,L12,D49,R71,U7,L72"
      (str/split #",")
      (wire-coords)))

(def test-wire4
  (-> "U62,R66,U55,R34,D71,R55,D58,R83"
      (str/split #",")
      (wire-coords)))

(defn common-points
  [w1 w2]
  (intersection (set (drop 1 w1)) (set (drop 1 w2))))

(defn distance-from-port [x y]
  (+ (Math/abs x) (Math/abs y)))

(defn distances [coords]
  (map #(distance-from-port (get % 0) (get % 1)) coords))

(defn min-distance [w1 w2]
  (apply min (distances (common-points w1 w2))))

(defn solve-part-1 []
  (min-distance wire1 wire2))

(defn get-cpd [p cpv]
  (loop [cur cpv]
    (cond
      (= (get (first cur) 0) p)
        (first cur)
      (nil? (first cur)) nil
      :else (recur (rest cur)))))

(defn add-distances [wire cpv]
  (keep-indexed
   (fn [idx c]
     (if-let [cp (get-cpd c cpv)]
       (let [d (get cp 1)
             nd (+ d idx)
             nv [(get cp 0) nd]]
         nv)
       nil))
   wire))

(defn to-cpv [w1 w2]
  (map (fn [p] [p 0]) (common-points w1 w2)))

(defn compute-runs [w1 w2]
  (let [cpv (to-cpv w1 w2)]
    (add-distances w2 (add-distances w1 cpv))))

(defn min-intersect-run [w1 w2]
  (let [common-runl (compute-runs w1 w2)
        first-dist (get (first common-runl) 1)]
    (reduce
     (fn [acc b]
       (let [d (get b 1)]
         (if (< d acc)
           d
           acc)))
     first-dist
     common-runl)))

(defn solve-part-2 []
  (min-intersect-run wire1 wire2))
