(ns aoc.day-10
  (:require [clojure.java.io :as io]))

(defn small-input []
  (-> "aoc-day-10-small.txt"
      io/resource
      io/reader
      line-seq))

(defn input []
  (-> "aoc-day-10.txt"
      io/resource
      io/reader
      line-seq))

(defn p1-parse-input [lines]
  (for [line lines]
    (let [[px py vx vy] (->> line
                             (re-seq #"-?\d+")
                             (map #(Integer/parseInt %)))]
      {:pos-x px
       :pos-y py
       :vlcty-x vx
       :vlcty-y vy})))

(defn p1-move [{:keys [vlcty-x vlcty-y] :as point}]
  (-> point
      (update :pos-x + vlcty-x)
      (update :pos-y + vlcty-y)))

(defn p1-print-matrix-size [points]
  (let [xs (->> points (map :pos-x))
        ys (->> points (map :pos-x))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (println {:x (- max-x min-x)
              :y (- max-y min-y)}))
  points)

(comment ;; p1-solver

  (require '[oz.core :as oz])
  (oz/start-plot-server!)

  (defn plot-matrix [points]
    (oz/v! {:data {:values points}
            :encoding {:x {:field "pos-x"
                           :type "quantitative"}
                       :y {:field "pos-y"
                           :type "quantitative"}}
            :width 1024
            :height 800
            :mark "point"})
    points)

  (loop [seconds 0
         points (p1-parse-input (input))]
    (let [max-x (->> points (map :pos-x) (apply max))]
      (if (< 205 max-x)
        (recur (inc seconds) (map p1-move points))
        (do
          (plot-matrix points)
          seconds)))))
