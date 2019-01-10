(ns aoc.day-21
  (:require [aoc.day-19 :as d19]
            [clojure.java.io :as io]))

(defn input []
  (-> "aoc-day-21-large.txt"
      io/resource
      io/reader
      line-seq))

(defn p1-solution []
  (-> (input)
      (d19/parse-input)
      (d19/run-cpu (fn [x] (= 28 (:ip x))))
      (get-in [:regs 3])))

(defn p2-solver []
  ;; SLOW!
  (let [cpu (-> (input)
                (d19/parse-input))]
    (loop [cpu cpu
           res []]
      (let [r3 (get-in cpu [:regs 3])]
        (println "R3:" r3 "Steps:" (:steps cpu))
        (if ((set res) r3)
          (last res)
          ;; Run cpu with breakpoint at instr 28
          (recur (d19/run-cpu (d19/step cpu) (fn [x] (= 28 (:ip x))))
                 (conj res r3)))))))
