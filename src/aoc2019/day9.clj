(ns aoc2019.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def pi-val (atom "1"))

(def p-out (atom 0))

(def rbase (atom 0))

(def tp-1 (str/split "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" #","))

(def tp-2 (str/split "1102,34915192,34915192,7,4,7,99,0" #","))

(def tp-3 (str/split "104,1125899906842624,99" #","))

(defn geti [coll x]
  (Long/parseLong (get coll x)))

(def input
  (-> (io/resource "day9input")
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
        xv (cond
             (= (:mode1 op) 0) (geti mem x)
             (= (:mode1 op) 1) x
             (= (:mode1 op) 2) (geti mem (+ x @rbase)))
        yv (cond
             (= (:mode2 op) 0) (geti mem y)
             (= (:mode2 op) 1) y
             (= (:mode2 op) 2) (geti mem (+ y @rbase)))]
    {:x xv :y yv :ap ap}))

(defn get-jmp-params [op ptr mem]
  (let [x (geti mem (+ ptr 1))
        ap (geti mem (+ ptr 2))
        xv (cond
             (= (:mode1 op) 0) (geti mem x)
             (= (:mode1 op) 1) x
             (= (:mode1 op) 2) (geti mem (+ x @rbase)))
        apv (cond
              (= (:mode2 op) 0) (geti mem ap)
              (= (:mode2 op) 1) ap
              (= (:mode2 op) 2) (geti mem (+ ap @rbase)))]
    {:x xv :ap apv}))

(defn get-param [op ptr mem]
  (let [x (geti mem (+ ptr 1))]
    (cond
      (= (:mode1 op) 0) (geti mem x)
      (= (:mode1 op) 1) x
      (= (:mode1 op) 2) (geti mem (+ x @rbase)))))

(defn ex-add [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (+ (:x params) (:y params))))))

(defn ex-mul [op ptr mem]
  (let [params (get-params op ptr mem)]
    (assoc mem (:ap params) (str (* (:x params) (:y params))))))

(defn ex-sto [op ptr mem]
  (let [x @pi-val
        ap (geti mem (+ ptr 1))]
    (assoc mem ap (str x))))

(defn ex-out [op ptr mem]
   (let [xv (get-param op ptr mem)]
    (reset! p-out xv)
    (println "OUTPUT: " xv)
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

(defn ex-arb [op ptr mem]
  (let [param (get-param op ptr mem)]
    (reset! rbase (+ @rbase param))
    mem))

(defn eval-op [op ptr mem]
  (try
    (cond
      (= (:opcode op) 1) {:result (ex-add op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 2) {:result (ex-mul op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 3) {:result (ex-sto op ptr mem) :ptr (+ ptr 2)}
      (= (:opcode op) 4) {:result (ex-out op ptr mem) :ptr (+ ptr 2)}
      (= (:opcode op) 5) {:result mem :ptr (ex-jit op ptr mem)}
      (= (:opcode op) 6) {:result mem :ptr (ex-jif op ptr mem)}
      (= (:opcode op) 7) {:result (ex-lt op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 8) {:result (ex-eq op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 9) {:result (ex-arb op ptr mem) :ptr (+ ptr 2)})
    (catch Exception e
      (.printStackTrace e)
      (println (str "OP:" op ",ptr:" ptr))
      (println (str "MEM AT PTR:" (get mem ptr) ","
                    (get mem (+ ptr 1)) ","
                    (get mem (+ ptr 2)) ","
                    (get mem (+ ptr 3))))
      (println (str "RELBASE: " @rbase))
      (print (str "FULL MEM:" mem)))))

(defn read-program [prog sp]
  (reset! rbase 0)
  (loop [ptr sp
         mem (into [] (concat prog (map str (take 100 (cycle "0")))))]
    (let [this-op (parse-op (get mem ptr))]
      (cond
        (= (:opcode this-op) 99)
          {:signal "HALT" :mem (take 5 mem) :ptr ptr}
        ;; (= (:opcode this-op) 4)
        ;;   (let [result (eval-op this-op ptr mem)]
        ;;     {:signal "OUTPUT" :mem (:result result) :ptr (:ptr result)})
        :else (let [result (eval-op this-op ptr mem)]
                (recur (:ptr result) (:result result)))))))

