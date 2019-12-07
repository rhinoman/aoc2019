(ns aoc2019.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def pi-vals (atom ["0" "0"]))

(def p-out (atom 0))

(def tp-1 ["1002" "4" "3" "4" "33"])

(def tp-2 ["1101" "100" "-1" "4" "0"])

(def tp-3 (str/split "3,9,8,9,10,9,4,9,99,-1,8" #","))

(def tp-4 (str/split "3,9,7,9,10,9,4,9,99,-1,8" #","))

(def tp-5 (str/split "3,3,1108,-1,8,3,4,3,99" #","))

(def tp-6 (str/split "3,3,1107,-1,8,3,4,3,99" #","))

(def tp-7 (str/split "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9" #","))

(def tp-8 (str/split "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" #","))

(def tp-9 (str/split "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" #","))

(def tp-10 (str/split "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" #","))

(def tp-11 (str/split "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" #","))

(defn geti [coll x]
  (Long/parseLong (get coll x)))

(def input
  (-> (io/resource "day7input")
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
  (let [x (first @pi-vals)
        ap (geti mem (+ ptr 1))]
    (reset! pi-vals (rest @pi-vals))
    (assoc mem ap (str x))))

(defn ex-out [op ptr mem]
  (let [ap (geti mem (+ ptr 1))
        xv (if (= (:mode1 op) 1) ap (geti mem ap))]
    (reset! p-out xv)
    (println "OUTPUT: " @p-out)
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
  (try
    (cond
      (= (:opcode op) 1) {:result (ex-add op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 2) {:result (ex-mul op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 3) {:result (ex-sto op ptr mem) :ptr (+ ptr 2)}
      (= (:opcode op) 4) {:result (ex-out op ptr mem) :ptr (+ ptr 2)}
      (= (:opcode op) 5) {:result mem :ptr (ex-jit op ptr mem)}
      (= (:opcode op) 6) {:result mem :ptr (ex-jif op ptr mem)}
      (= (:opcode op) 7) {:result (ex-lt op ptr mem) :ptr (+ ptr 4)}
      (= (:opcode op) 8) {:result (ex-eq op ptr mem) :ptr (+ ptr 4)})
    (catch Exception e
      (.printStackTrace e)
      (println (str "OP:" op ",ptr:" ptr))
      (println (str "MEM AT PTR:" (get mem ptr) ","
                    (get mem (+ ptr 1)) ","
                    (get mem (+ ptr 2)) ","
                    (get mem (+ ptr 3))))
      (print (str "FULL MEM:" mem)))))

(defn read-program [prog sp]
  (loop [ptr sp
         mem prog]
    (let [this-op (parse-op (get mem ptr))]
      (cond
        (= (:opcode this-op) 99)
          {:signal "HALT" :mem mem :ptr ptr}
        (= (:opcode this-op) 4)
          (let [result (eval-op this-op ptr mem)]
            {:signal "OUTPUT" :mem (:result result) :ptr (:ptr result)})
        :else (let [result (eval-op this-op ptr mem)]
                (recur (:ptr result) (:result result)))))))

(defn run-amp [phase input prog ptr]
  (reset! pi-vals [(str phase) (str input)])
  (read-program prog ptr))

(defn run-amp-seq [input prog ptr]
  (reset! pi-vals [(str input)])
  (println (str "INPUT:" @pi-vals))
  (read-program prog ptr))

(defn run-amps [phase-vals prog]
  (reset! p-out 0)
  (loop [pv phase-vals]
    (let [tp (first pv)]
      (cond
        (nil? tp) @p-out
        :else
          (do
            (run-amp tp @p-out prog 0)
            (recur (rest pv)))))))

(defn run-amps-cont [phase-vals prog]
  (reset! p-out 0)
  (let [first-run (into [] (map (fn [i]
                                  (let [result (run-amp (get phase-vals i)
                                                        @p-out
                                                        prog
                                                        0)]
                                    {:mem (:mem result) :ptr (:ptr result)}))
                                (range 0 5)))]
    (loop [pvi 0
           amp-state first-run]
      (let [tp (if (> pvi 4) 0 pvi)
            st (get amp-state tp)
            result (run-amp-seq @p-out (:mem st) (:ptr st))]
        (println (str "SIGNAL:" (:signal result) ",PTR:" (:ptr result)))
        (cond
          (= (:signal result) "HALT")
            @p-out
          (= (:signal result) "OUTPUT")
            (do
              (let [next-tp (if (= tp 4) 0 (inc tp))]
                (recur
                 next-tp
                 (assoc amp-state tp {:mem (:mem result) :ptr (:ptr result)}))))
            :else
              (println "I DON'T KNOW WHAT TO DO"))))))

(defn solve-part-1 [prog]
  (let [pp (combo/permutations [0 1 2 3 4])
        sols (map (fn [p] (run-amps p prog)) pp)]
    (apply max sols)))

(defn solve-part-2 [prog]
  (let [pp (combo/permutations [5 6 7 8 9])
        sols (map (fn [p] (run-amps-cont p prog)) pp)]
    (apply max sols)))
