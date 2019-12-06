(ns aoc2019.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
(def input-value (atom 0))

(def tp-1 ["1002" "4" "3" "4" "33"])

(def tp-2 ["1101" "100" "-1" "4" "0"])

(def tp-3 (str/split "3,9,8,9,10,9,4,9,99,-1,8" #","))

(def tp-4 (str/split "3,9,7,9,10,9,4,9,99,-1,8" #","))

(def tp-5 (str/split "3,3,1108,-1,8,3,4,3,99" #","))

(def tp-6 (str/split "3,3,1107,-1,8,3,4,3,99" #","))

(def tp-7 (str/split "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" #","))

(defn geti [coll x]
  (Integer/parseInt (get coll x)))

(def input
  (-> (io/resource "day5input")
      (slurp)
      (str/split #",")))

(defn parse-op
  [operation]
  (let [p-op (str "0000" operation) ;;Padding
        len (count p-op)
        op (Integer/parseInt (subs p-op (- len 2) len))
        mode1 (Integer/parseInt (or (subs p-op (- len 3) (- len 2)) "0"))
        mode2 (Integer/parseInt (or (subs p-op (- len 4) (- len 3)) "0"))
        mode3 (Integer/parseInt (or (subs p-op (- len 5) (- len 4)) "0"))]
    {:opcode op :mode1 mode1 :mode2 mode2 :mode3 mode3}))

(defn get-params [op ptr mem]
  (let [x (geti mem (+ ptr 1))
        y (geti mem (+ ptr 2))
        ap (geti mem (+ ptr 3))
        xv (if (= (:mode1 op) 1) x (geti mem x))
        yv (if (= (:mode2 op) 1) y (geti mem y))]
    {:x xv :y yv :ap ap}))

(defn get-jmp-params [op ptr mem]
  (let [x (geti mem (+ ptr 1))
        ap (geti mem (+ ptr 2))
        xv (if (= (:mode1 op) 1) x (geti mem x))
        apv (if (= (:mode2 op) 1) ap (geti mem ap))]
    {:x xv :ap apv}))


(defn ex-add [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (+ (:x params) (:y params))))))

(defn ex-mul [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (* (:x params) (:y params))))))

(defn ex-sto [op ptr mem]
  (let [x @input-value
        ap (geti mem (+ ptr 1))]
    (assoc mem ap (str x))))

(defn ex-out [op ptr mem]
  (let [ap (geti mem (+ ptr 1))
        xv (if (= (:mode1 op) 1) ap (geti mem ap))]
    (println (str "PRINT:" xv))
    mem))

(defn ex-jit [op ptr mem]
  (let [params (get-jmp-params op ptr mem)]
    (if (not= 0 (:x params))
      (:ap params)
      (+ ptr 3))))

(defn ex-jif [op ptr mem]
  (let [params (get-jmp-params op ptr mem)]
    (if (= 0 (:x params))
      (:ap params)
      (+ ptr 3))))

(defn ex-lt [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (if (< (:x params) (:y params)) 1 0)))))

(defn ex-eq [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (if (= (:x params) (:y params)) 1 0)))))

(defn eval-op [op ptr mem]
  (cond
    (= (:opcode op) 1) {:result (ex-add op ptr mem) :ptr (+ ptr 4)}
    (= (:opcode op) 2) {:result (ex-mul op ptr mem) :ptr (+ ptr 4)}
    (= (:opcode op) 3) {:result (ex-sto op ptr mem) :ptr (+ ptr 2)}
    (= (:opcode op) 4) {:result (ex-out op ptr mem) :ptr (+ ptr 2)}
    (= (:opcode op) 5) {:result mem :ptr (ex-jit op ptr mem)}
    (= (:opcode op) 6) {:result mem :ptr (ex-jif op ptr mem)}
    (= (:opcode op) 7) {:result (ex-lt op ptr mem) :ptr (+ ptr 4)}
    (= (:opcode op) 8) {:result (ex-eq op ptr mem) :ptr (+ ptr 4)}))

(defn read-program [prog iv]
  (reset! input-value (str iv))
  (loop [ptr 0
         mem prog]
    (let [this-op (parse-op (get mem ptr))]
      (if (= (:opcode this-op) 99)
        nil
        (let [result (eval-op this-op ptr mem)]
          (recur (:ptr result) (:result result)))))))

(defn solve-part-1 []
  (read-program input "1"))

(defn solve-part-2 []
  (read-program input "5"))
