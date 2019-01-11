(ns aoc.day-5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input []
  (->> "aoc-day-5.txt"
       io/resource
       io/reader
       slurp
       str/trim-newline))

(defn p1-reacts? [p1 p2]
  (and p1 p2
       (= 32 (Math/abs (- (int p1)
                          (int p2))))))

(defn p1-reducer [input]
  (->> input
       (reduce (fn [acc p]
                 (if (p1-reacts? (first acc) p)
                   (rest acc)
                   (cons p acc)))
               '())))

(defn p1-reduction []
  (p1-reducer (input)))

(defn p1-solution []
  (count p1-reduction))

(defn p2-mk-re [p]
  (re-pattern
   (str
    "[" (str/lower-case p) (str/upper-case p) "]")))

(defn p2-solver []
  (let [reduction (p1-reduction)]
    (->> reduction
         set
         (map str/lower-case)
         set
         (map (fn [p]
                 (let [re (p2-mk-re p)]
                   (-> (apply str reduction)
                       (str/replace re "")
                       p1-reducer
                       count))))
         (apply min))))
