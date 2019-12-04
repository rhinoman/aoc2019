(ns aoc2019.day4)

(def sp (range 156218 652528))

(defn is-solution? [x]
  (let [xstr (str x)]
    (loop [xs xstr
           has-dbl false
           is-inc true]
      (let [cur (first xs)
            nx (second xs)]
        (cond
          (nil? nx) (and is-inc has-dbl)
          (< (Character/digit nx 10) (Character/digit cur 10)) false
          :else (recur (rest xs) (or has-dbl (= nx cur)) is-inc))))))

(defn solve-part-1 []
  (let [solutions (keep (fn [x] (if (is-solution? x) x nil)) sp)]
    (count solutions)))

(defn add-run [x nx prev runs]
  (cond
    (= x nx prev) (assoc runs x 3)
    (= x nx) (assoc runs x 2)
    :else runs))

(defn eval-runs [runs]
  (> (count (filter (fn [x] (= 2 (get x 1))) runs)) 0))

(defn is-solution-2? [x]
  (let [xstr (str x)]
    (loop [xs xstr
           runs {}
           prev nil
           is-inc true]
      (let [cur (first xs)
            nx (second xs)]
        (cond
          (nil? nx) (and is-inc (eval-runs runs))
          (< (Character/digit nx 10) (Character/digit cur 10)) false
          :else (recur
                 (rest xs)
                 (add-run cur nx prev runs)
                 cur
                 is-inc))))))

(defn solve-part-2 []
  (let [solutions (keep (fn [x] (if (is-solution-2? x) x nil)) sp)]
    (count solutions)))
