(ns aoc.day-6
  (:require [clojure.java.io :as io]))

(defn input []
  (->> "aoc-day-6.txt"
       io/resource
       io/reader
       line-seq
       (map #(re-seq #"\d+" %))
       (map (partial map clojure.edn/read-string))))

(defn small-input []
  [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn taxi-circle [radius]
  (let [range (range (inc radius))
        reverse (reverse range)]
    (distinct
     (concat
      (map vector range reverse)
      (map vector reverse (map - range))
      (map vector (map - range) (map - reverse))
      (map vector (map - reverse) range)))))

(defn offset-circle [c [ox oy]]
  (map (fn [[cx cy]]
         [(+ cx ox) (+ cy oy)])
       c))

(defn has-points-on-edge? [min-x max-x min-y max-y [x points]]
  (when (->> points
             (map first)
             (some (fn [[x y]]
                     (or (= min-x x)
                         (= max-x x)
                         (= min-y y)
                         (= max-y y)))))
    true))

(defn outside-box? [min-x max-x min-y max-y [x y]]
  (or (< x min-x)
      (< y min-y)
      (< max-x x)
      (< max-y y)))

(defn p1-point-reducer [n circle remove-fn merge-fn areas input-point]
  (->> (offset-circle circle input-point)
       (remove remove-fn)
       (reduce
        (fn [areas circle-point]
          (merge-with
           merge-fn
           areas
           {circle-point {:closest-neighbour input-point
                          :distance n}}))
        areas)))

(defn p1-merge-fn [pre new]
  (if (or (= (:distance pre) (:distance new))
          (= :collision pre))
    :collision
    (if (< (:distance pre) (:distance new))
      pre
      new)))

(defn p1-solver [input]
  (let [xs (->> input (map first))
        ys (->> input (map second))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)

        start-areas (->> input
                         (map (fn [point]
                                [point {:closest-neighbour point
                                        :distance 0}]))
                         (into {}))
        areas (reduce
               (fn [areas n]
                 (let [circle (taxi-circle n)
                       outside-box? (partial outside-box? min-x max-x min-y max-y)
                       areas (->> input
                                  (reduce
                                   (partial p1-point-reducer n circle outside-box? p1-merge-fn)
                                   areas))]
                   areas))
               start-areas
               (range 1 (max (- max-x min-x)
                             (- max-y min-y))))]
    (->> areas
         (group-by (comp :closest-neighbour val))
         (remove (partial has-points-on-edge? min-x max-x min-y max-y))
         (sort-by (comp count second))
         last
         second
         count)))

(defn p1-solution []
  (p1-solver (input)))
