(ns aoc2019.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


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

(def tp-2 (-> ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....X...###..
..#.#.....#....##"
  (asteroid-locations)))

(def tp-3 (-> ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"
(asteroid-locations)))

(def tp-2-station [8 3])

(defn compute-theta [origin pt]
  (let [x0 (first origin)
        y0 (second origin)]
    (Math/atan2 (- (second pt) y0) (- (first pt) x0))))

(defn compute-thetas [cs data]
  (let [fdata (filter #(not= cs %) data)]
    (map #(compute-theta cs %) fdata)))

(defn count-visible [cs data]
  (count (distinct (compute-thetas cs data))))

(defn compute-visibility [data]
  (map (fn [pt] {:pt pt :num-visible (count-visible pt data)}) data))

(defn solve-part-1 [data]
  (let [viz (compute-visibility data)]
    (reduce (fn [acc b] (if (> (:num-visible b) (:num-visible acc)) b acc)) (first viz) viz)))

(def p1-station [17 22])



(defn distance [pt1 pt2]
  (Math/sqrt (+ (Math/pow (- (first pt2) (first pt1)) 2)
                (Math/pow (- (second pt2) (second pt1)) 2))))

(defn compute-thetas-distance [origin data]
  (let [fdata (filter #(not= origin %) data)]
    (map (fn [pt] {:theta (compute-theta origin pt)
                   :pt pt
                   :distance (distance pt origin)})
         fdata)))

(defn fire! [origin theta data]
  "Fires the laser once.
   Returns the destroyed asteroid coord,
   And the data minus the destroyed asteroid"
  (let [possibilities (filter #(= (:theta %) theta) data)
        victim (first (sort-by :distance possibilities))]
    {:victim victim :data (filter #(not= victim %) data)}))

(defn order-targets [origin data]
  (let [start-az (- (/ Math/PI 2))
        od (into [] (sort-by :theta (compute-thetas-distance origin data)))]
    (loop [cur (first od)
           rst (rest od)
           ans od]
      (cond
        (nil? cur) ans
        (< (:theta cur) start-az)
          (recur (first rst) (rest rst) (conj (into [] (rest ans)) cur))
        :else (recur (first rst) (rest rst) ans)))))

(defn solve-part-2 [station data]
  (let [targets (order-targets station data)
        thetas (distinct (map #(:theta %) targets))]
    (loop [victims targets
           destroyed []
           tl thetas]
      (let [next-theta (first tl)
            vlist (filter #(= next-theta (:theta %)) victims)
            dead (first (sort-by :distance vlist))
            alive (filter #(not= (:pt dead) (:pt %)) victims)]
        (println next-theta)
        (cond
          (<= (count alive) 0) (conj destroyed dead)
          (nil? next-theta) (recur alive destroyed thetas)
          (nil? dead) (recur alive destroyed (rest tl))
          ;(nil? next-theta) (recur alive destroyed (:theta (first alive)))
          :else (recur alive
                       (conj destroyed dead)
                       (rest tl)))))))
