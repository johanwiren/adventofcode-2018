(ns aoc.day-16
  (:require [clojure.java.io :as io]))

(defn make-cpu
  ([] (make-cpu 0 0 0 0))
  ([r1 r2 r3 r4]
   [r1 r2 r3 r4]))

(defn rr-fn [f]
  (fn [cpu a b c]
    (assoc cpu c (f (get cpu a)
                    (get cpu b)))))

(defn ri-fn [f]
  (fn [cpu a b c]
    (assoc cpu c (f (get cpu a)
                    b))))

(defn ir-fn [f]
  (fn [cpu a b c]
    (assoc cpu c (f a
                    (get cpu b)))))

(defn gt-fn [x y]
  (if (< y x) 1 0))

(defn eq-fn [x y]
  (if (= x y) 1 0))

(def opcodes
  {:addr (rr-fn +)
   :addi (ri-fn +)
   :mulr (rr-fn *)
   :muli (ri-fn *)
   :banr (rr-fn bit-and)
   :bani (ri-fn bit-and)
   :borr (rr-fn bit-or)
   :bori (ri-fn bit-or)
   :setr (rr-fn (fn [x _] x))
   :seti (ir-fn (fn [x _] x))
   :gtir (ir-fn gt-fn)
   :gtri (ri-fn gt-fn)
   :gtrr (rr-fn gt-fn)
   :eqir (ir-fn eq-fn)
   :eqri (ri-fn eq-fn)
   :eqrr (rr-fn eq-fn)})

(defn input [size]
  (case size
    :small ["Before: [3, 2, 1, 1]"
            "9 2 1 2"
            "After:  [3, 2, 2, 1]"]
    :large (->> "aoc-day-16-large.txt"
                io/resource
                io/reader
                line-seq
                (take 3260))))

(defn parse-line [line]
  (mapv #(Integer/parseInt %)
        (re-seq #"\d+" line)))

(defn parse-input [input]
  (->> input
       (partition-all 4)
       (map (fn [lines]
              {:init (parse-line (first lines))
               :instr (parse-line (second lines))
               :result (parse-line (nth lines 2))}))))

(defn apply-sample [{:keys [init instr result] :as sample}]
  (assoc sample

         :matching-opcodes
         (->> opcodes
              (filter (fn [[name op-fn]]
                        (= (apply (partial op-fn init) (rest instr))
                           result)))
              (map first))

         :opcode-nr (first instr)))

(defn p1-solver []
  (->> (input :large)
       parse-input
       (map apply-sample)
       (filter (fn [x] (< 2 (count (:matching-opcodes x)))))
       count))

(defn make-opcode-map [input]
  (->> input
       parse-input
       (map apply-sample)
       (map (juxt :opcode-nr :matching-opcodes))
       (map (partial apply hash-map))
       (apply (partial merge-with (fn [x y] (distinct (concat x y)))))))

(defn shrink-opcodes [opcode-map]
  (let [sure-opcodes (->> opcode-map
                          vals
                          (filter #(= 1 (count %)))
                          flatten
                          set)]
    (reduce-kv (fn [acc k v]
                 (assoc acc k (if (= 1 (count v))
                                v
                                (remove sure-opcodes v))))
               {}
               opcode-map)))

(def opcode-nr-to-opcode
  (delay
   (let [opcode-map (make-opcode-map (input :large))]
     (loop [opcode-map opcode-map]
       (if (->> opcode-map
                vals
                (map count)
                (remove #{1})
                seq)
         (recur (shrink-opcodes opcode-map))
         (->> opcode-map
              (map (fn [[k v]] [k (first v)]))
              (into {})))))))

(defn p2-input []
  (->> "aoc-day-16-large.txt"
       io/resource
       io/reader
       line-seq
       (drop 3262)
       (map parse-line)))

(defn p2-solver []
  (->> (p2-input)
       (reduce (fn [regs [op-nr & args]]
                 (let [opcode (get @opcode-nr-to-opcode op-nr)
                       opcode-fn (get opcodes opcode)]
                   (apply (partial opcode-fn regs) args)))
               (make-cpu))
       first))
