(ns aoc.day-11)

(def input 1955)

(defn power [x y serial]
  (let [rack-id (+ x 10)]
    (-> (* rack-id y)
        (+ serial)
        (* rack-id)
        (quot 100)
        (mod 10)
        (- 5))))

(defn fuel-cells [input]
  (->>
   (for [y (range 1 (inc 300))
         x (range 1 (inc 300))]
     [[x y] (power x y input)])
   (into {})))

(defn cells-3x3 [fuel-cells]
  (->>
   (for [y (range 1 (inc 298))
         x (range 1 (inc 298))]
     (let [power (->> (for [cell-y (range 3)
                            cell-x (range 3)]
                        (get fuel-cells [(+ x cell-x) (+ y cell-y)]))
                      (apply +))]
       [[x y] power]))
   (into {})))

(defn p1-solution []
  (->> (fuel-cells input)
       cells-3x3
       (sort-by val)
       last))

