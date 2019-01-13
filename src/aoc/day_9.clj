(ns aoc.day-9)

(defn rotate-marbles [[left right]]
  (if (empty? right)
    [`() (reverse left)]
    [left right]))

(defn insert-marble [marbles marble]
  (let [[left [first-right & more-right]] (rotate-marbles marbles)]
    [(cons marble
           (if first-right
             (cons first-right left)
             left))
     more-right]))

(defn take-marble [[left right]]
  (loop [steps 7
         left left
         right right]
    (if (empty? left)
      (recur steps (reverse right) `())
      (if (zero? steps)
        [(first left) [(cons (first right) (rest left)) (rest right)]]
        (recur (dec steps) (rest left) (cons (first left) right))))))

(defn play-game [players max-marble-score]
  (loop [scores {}
         marbles [`(0) `()]
         marble-score 1]
    (if (< max-marble-score marble-score)
      {:scores scores
       :marbles marbles}
      (if (zero? (rem marble-score 23))
        (let [[taken-marble marbles] (take-marble marbles)
              total-score (+ taken-marble marble-score)
              player (rem marble-score players)]
          (recur (update scores player (fnil + 0) total-score)
                 marbles
                 (inc marble-score)))
        (recur scores
               (insert-marble marbles marble-score)
               (inc marble-score))))))

(defn solve [players max-marble-score]
  (->> (play-game players max-marble-score)
       :scores
       (sort-by second)
       last
       last))

(defn p1-solution []
  (solve 468 71010))

(defn p2-solution []
  (solve 468 (* 71010 100)))
