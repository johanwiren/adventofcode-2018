(ns aoc.day-2
  (:require [clojure.java.io :as io]))

(def p1-input (-> "aoc-day-2.txt" io/resource io/reader line-seq))

(defn p1-count-box [box]
  (->> box
       frequencies
       vals
       ((juxt (partial some #{2}) (partial some #{3})))
       (map #(if (not (or (nil? %) (zero? %)))
               1
               0))))

(def p1-solution
  (delay
   (->> p1-input
        (map p1-count-box)
        (apply map vector)
        (map (partial reduce +))
        (apply *))))

(defn p2-found-packages? [p1 p2]
  (->> (map vector p1 p2)
       (map (partial apply =))
       (filter false?)
       count
       (#{1})))

(defn p2-solver [box-numbers]
  (loop [head (first box-numbers)
         tail (next box-numbers)]
    (if-let [match (->> tail
                        (filter (partial p2-found-packages? head))
                        first)]
      (->> match
           (filter (set head))
           (apply str))
      (when (next tail)
        (recur (first tail) (next tail))))))

(def p2-solution (delay (p2-solver p1-input)))

