(ns aoc2019.day8
  (:require [clojure.java.io :as io]))

(def input
  (->> (io/resource "day8input")
      (slurp)
      (map #(Character/digit % 10))))

(def img-width 25)
(def img-height 6)

(def test-input '(1 2 3 4 5 6 7 8 9 0 1 2))

(def test-width 3)
(def test-height 2)

(def test-input-2 '(0 2 2 2 1 1 2 2 2 2 1 2 0 0 0 0))

(def test-width-2 2)
(def test-height-2 2)

(defn layers [data w h]
  (partition (* w h) data))

(defn pixels [data w h]
  (map #(partition w %) (layers data w h)))

(defn count-digits [digit layer]
  (reduce (fn [acc b] (if (= b digit) (inc acc) acc)) 0 layer))

(defn lwf [digit lv]
  (reduce (fn [acc l]
            (let [nd (count-digits digit l)]
              (if (< nd (:nd acc)) {:nd nd :layer l} acc)))
          {:nd (count-digits digit (first lv)) :layer (first lv)}
          lv))

(defn solve-part-1 []
  (let [lwf-z (:layer (lwf 0 (layers input img-width img-height)))
        n-ones (count-digits 1 lwf-z)
        n-twos (count-digits 2 lwf-z)]
    (* n-ones n-twos)))

(defn flatten-layers [lv w h]
  (map (fn [i] (map #(get (into [] %) i) lv)) (range (* w h))))

(defn get-pixel-color [pv]
  (loop [lc (first pv)
         rst (rest pv)]
    (if (< lc 2)
      lc
      (recur (first rst) (rest rst)))))

(defn flatten-image [lv w h]
  (let [fl (flatten-layers lv w h)]
    (map #(get-pixel-color %) fl)))

(defn disp-image [lv w h]
  (let [cv (flatten-image lv w h)
        rows (partition w cv)]
    (doseq [r rows] (println r))))

(defn solve-part-2 []
  (let [lz (layers input img-width img-height)]
    (disp-image lz img-width img-height)))
