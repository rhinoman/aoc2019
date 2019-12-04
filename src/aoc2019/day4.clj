(ns aoc2019.day4)

(defn num-to-digits
  [num]
  (loop [n num res []]
    (if (zero? n)
      res
      (recur (quot n 10) (cons (mod n 10) res)))))

(def sp (range 156218 652528))

(defn is-solution? [xstr]
  (loop [xs (num-to-digits xstr)
         has-dbl false
         is-inc true]
    (let [cur (first xs)
          nx (second xs)]
      (cond
        (nil? nx) (and is-inc has-dbl)
        (< nx cur) false
        :else (recur (rest xs) (or has-dbl (= nx cur)) is-inc)))))

(defn solve-part-1 []
  (reduce (fn [acc b] (if (is-solution? b) (inc acc) acc)) 0 sp))

(defn add-run [x nx prev runs]
  (cond
    (= x nx prev) (assoc runs x 3)
    (= x nx) (assoc runs x 2)
    :else runs))

(defn eval-runs [runs]
  (> (count (filter (fn [x] (= 2 (get x 1))) runs)) 0))

(defn is-solution-2? [xstr]
  (loop [xs (num-to-digits xstr)
         runs {}
         prev nil
         is-inc true]
    (let [cur (first xs)
          nx (second xs)]
      (cond
        (nil? nx) (and is-inc (eval-runs runs))
        (< nx cur) false
        :else (recur
               (rest xs)
               (add-run cur nx prev runs)
               cur
               is-inc)))))

(defn solve-part-2 []
  (reduce (fn [acc b] (if (is-solution-2? b) (inc acc) acc)) 0 sp))
