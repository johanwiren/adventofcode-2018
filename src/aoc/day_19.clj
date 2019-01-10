(ns aoc.day-19
  (:require [aoc.day-16 :as d16]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lanterna.screen :as screen]))

(defn input [size]
  (case size
    :small ["#ip 0"
            "seti 5 0 1"
            "seti 6 0 2"
            "addi 0 1 0"
            "addr 1 2 3"
            "setr 1 0 0"
            "seti 8 0 4"
            "seti 9 0 5"]
    :large (-> "aoc-day-19-large.txt"
               io/resource
               io/reader
               line-seq)))

(defn make-cpu
  ([ip-reg instructions]
   (make-cpu ip-reg instructions [0 0 0 0 0 0]))
  ([ip-reg instructions registers]
   {:state :ready
    :ip 0
    :regs registers
    :ip-reg ip-reg
    :instructions instructions
    :steps 0}))

(defn parse-input
  ([lines]
   (parse-input make-cpu lines))
  ([make-cpu-fn lines]
   (let [ip-reg (-> lines first (str/split #" ") last clojure.edn/read-string)
         instructions (->> lines
                           rest
                           (map #(str/split % #" "))
                           (map (fn [line-items]
                                  (-> (mapv clojure.edn/read-string line-items)
                                      (update 0 keyword))))
                           vec)]
     (make-cpu-fn ip-reg instructions))))

(defn copy-in [m s-path d-path]
  (let [val (get-in m s-path)]
    (assoc-in m d-path val)))

(defn do-instr [{:keys [ip regs instructions] :as cpu}]
  (let [[op & op-args] (get instructions ip)
        op-fn (get d16/opcodes op)]
    (update cpu :regs #(apply (partial op-fn %) op-args))))

(defn step [{:keys [ip regs ip-reg instructions] :as cpu}]
  (let [cpu (-> cpu
                (assoc-in [:regs ip-reg] ip)
                do-instr
                (copy-in [:regs ip-reg] [:ip])
                (update :ip inc)
                (update :steps inc))]
    (if (get instructions (:ip cpu))
      cpu
      (assoc cpu :state :halted))))

(defn run-cpu
  ([cpu])
  ([cpu break-fn]
   (loop [cpu cpu]
     (if (or (not= :ready (:state cpu))
             (break-fn cpu))
       cpu
       (recur (step cpu))))))

(defn p1-solver []
  (let [cpu (->> (input :large)
                 parse-input)]
    (run-cpu cpu)))

(defn show-jumps [{:keys [ip-reg] :as cpu} [instr a b c]]
  (if (and (= ip-reg c)
           (#{:addi :addr :seti} instr))
    (case instr
      :setr [:jarg a]
      :seti [:jmpj a]
      :addi [:jrin b]
      :addr [:jrrg a])
    [instr a b c]))

(defn clear-screen [screen]
  (let [[size-x size-y] (screen/get-size screen)
        blank-line (apply str (repeat size-x \space))]
    (doseq [y (range size-y)]
      (screen/put-string screen 0 y blank-line))))

(defn draw-cpu
  "You will need a screen for this:

  (def screen (screen/get-screen :swing {:cols 80 :rows 40}))
  (screen/start screen)

  "
  ([screen cpu]
   (draw-cpu screen cpu step))
  ([screen {:keys [ip] :as cpu} step-fn]
   (clear-screen screen)
   (loop [[reg & more] (:regs cpu)
          row 0]
     (screen/put-string screen 0 row (str [row reg]))
     (if more
       (recur more (inc row))))

   (loop [[instr & more] (:instructions cpu)
          row 0]
     (let [instr-str (->> instr
                          (show-jumps cpu)
                          (str (if (= ip row) ">" " ")))]
       (screen/put-string screen 50 row instr-str))
     (if more
       (recur more (inc row))))
   (screen/redraw screen)

   (let [[action cpu]
         (case (screen/get-key-blocking screen)
           \q (do
                (screen/put-string screen 0 39 "Quit")
                (screen/redraw screen)
                [:quit cpu])
           \n [:continue (step cpu)]
           \k [:continue (run-cpu cpu
                                  (fn [inner-cpu]
                                    (<= (+ (:steps cpu) 1000)
                                        (:steps inner-cpu))))]
           \m [:continue (run-cpu cpu
                                  (fn [inner-cpu]
                                    (<= (+ (:steps cpu) 1000000)
                                        (:steps inner-cpu))))]
           [:continue (step-fn cpu)])]
     (if (= :continue action)
       (recur screen cpu step-fn)
       cpu))))

(defn p2-load-cpu []
  (->> (input :large)
       (parse-input (fn [ip-reg instructions]
                      (make-cpu ip-reg instructions [1 0 0 0 0 0])))))

(defn p2-solution []
  (let [cpu (->> (input :large)
                 (parse-input (fn [ip-reg instructions]
                                (make-cpu ip-reg instructions [1 0 0 0 0 0]))))
        magic-val (-> cpu
                      (run-cpu (fn [x] (= 1 (:ip x))))
                      :regs
                      second)]
    (->> (range 1 (inc magic-val))
         (filter (fn [x] (zero? (rem magic-val x))))
         (apply +))))
