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

(defn is-solution-2? [x]
  (let [xstr (str x)]
    (loop [xs xstr
           has-dbl 0
           is-inc true]
      (let [cur (first xs)
            nx (second xs)
            tr (second (rest xs))
            dbl (if (= cur nx) 1 0)
            tpl (if (= cur nx tr) -2 0)]
        (cond
          (nil? nx) (and is-inc (> has-dbl 0))
          (< (Character/digit nx 10) (Character/digit cur 10)) false
          :else (recur (rest xs) (+ has-dbl dbl tpl) is-inc))))))

(defn solve-part-2 []
  (let [solutions (keep (fn [x] (if (is-solution-2? x) x nil)) sp)]
    (count solutions)))
