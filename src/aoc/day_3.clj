(ns aoc.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input []
  (->> "aoc-day-3.txt"
       io/resource
       io/reader
       line-seq
       (map (comp (partial map #(Integer/parseInt %))
                  (partial remove #{""})
                  #(str/split % #"[^\d]")))))

(defn p1-get-points [[claim start-x start-y size-x size-y]]
  (for [y (range start-y (+ start-y size-y))
        x (range start-x (+ start-x size-x))]
    [x y]))

(defn p1-overlapping-points []
  (->> (input)
       (mapcat p1-get-points)
       frequencies
       (filter #(< 1 (second %)))
       (map first)))

(defn p1-solution []
  (count (p1-overlapping-points)))

(defn p2-solution []
  (let [overlapping? (set (p1-overlapping-points))]
    (->> (for [claim (input)]
           (when (zero? (->> (p1-get-points claim)
                             (filter overlapping?)
                             count))
             claim))
         (remove nil?))))
