(ns aoc2019.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-input ["COM)B"
                 "B)C"
                 "C)D"
                 "D)E"
                 "E)F"
                 "B)G"
                 "G)H"
                 "D)I"
                 "E)J"
                 "J)K"
                 "K)L"])

(def test-input-2 ["COM)B"
                   "B)C"
                   "C)D"
                   "E)F"
                   "B)G"
                   "G)H"
                   "D)E"
                   "D)I"
                   "E)J"
                   "J)K"
                   "K)L"])

(def test-input-3 ["COM)B"
                   "B)C"
                   "C)D"
                   "D)E"
                   "E)F"
                   "B)G"
                   "G)H"
                   "D)I"
                   "E)J"
                   "J)K"
                   "K)L"
                   "K)YOU"
                   "I)SAN)"])

(def input
  (-> (io/resource "day6input")
      (slurp)
      (str/split-lines)))

(defn build-list
  [data]
  (reduce
   (fn [acc b]
     (let [x (str/split b #"\)")]
       (conj acc {:p (get x 0) :s (get x 1)})))
   ()
   data))

(defn count-run
  [node lst]
  (loop [xs lst
         n node
         cnt 0]
    (let [cur (first xs)]
      (cond
        (nil? cur)
          cnt
        (= (:s cur) n)
          (recur lst (:p cur) (inc cnt))
        :else
          (recur (rest xs) n cnt)))))

(defn solve-part-1
 [data]
 (let [list (build-list data)]
   (reduce (fn [acc b] (+ acc (count-run (:s b) list))) 0 list)))

(defn find-runs
  [node lst]
  (loop [xs lst
         n node
         cnt 0
         path {}]
    (let [cur (first xs)]
      (cond
        (nil? cur)
          path
        (= (:s cur) n)
        (do
          (if (contains? path n) (println "DOUBLE!"))
          (recur lst (:p cur) (inc cnt) (assoc path n cnt)))
        :else
          (recur (rest xs) n cnt path)))))

(defn solve-part-2
  [data]
  (let [list (build-list data)
        yrun (find-runs "YOU" list)
        srun (find-runs "SAN" list)
        runlengths (keep (fn [[k v]]
                           (if (contains? srun k)
                             (+ (dec v) (dec (get srun k)))))
                         yrun)]
    (apply min runlengths)))
