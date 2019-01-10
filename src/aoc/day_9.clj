(ns aoc.day-9)

(defn p1-game [players marbles]
  {:players players
   :scores {}
   :marbles `(0)
   :round 0
   :current-marble-pos 0
   :marbles-to-play (range 1 (inc marbles))
   :player-turns (cycle (range 1 (inc players)))})

(defn p1-split-at [at coll]
  (let [size (count coll)
        pos (cond
              (< size at) (- at size)
              (> 0 at) (+ size at)
              :esle at)]
    (split-at pos coll)))

(defn p1-insert-marble [marbles current-marble-pos new-marble]
  (let [[left right] (p1-split-at (+ current-marble-pos 2) marbles)
        new-marble-pos (count left)]
    [new-marble-pos (concat left [new-marble] right)]))

(defn p1-game-insert-marble [{:keys [marbles-to-play marbles current-marble-pos] :as game}]
  (let [new-marble (first marbles-to-play)
        [new-marble-pos new-marbles] (p1-insert-marble marbles current-marble-pos new-marble)]
    (-> game
        (assoc :marbles new-marbles
               :current-marble-pos new-marble-pos))))

(defn p1-remove-marble [marbles marble-pos]
  (let [[left right] (p1-split-at marble-pos marbles)]
    [(last left) (dec (count left)) (concat (butlast left) right)]))

(defn p1-game-remove-marble [{:keys [current-marble-pos marbles] :as game}]
  (let [[removed-marble new-marble-pos new-marbles] (p1-remove-marble marbles (- current-marble-pos 6))]
    (-> game
        (assoc :marbles new-marbles
               :current-marble-pos new-marble-pos
               :removed-marble removed-marble))))

(defn p1-game-count-scores [{:keys [removed-marble marbles-to-play player-turns] :as game}]
  (let [current-player (first player-turns)
        score (+ removed-marble (first marbles-to-play))]
    (update-in game [:scores current-player] (fnil + 0) score)))

(defn p1-play-round [{:keys [round] :as game}]
  (let [action (if (zero? (rem (inc round) 23))
                 (comp p1-game-count-scores
                       p1-game-remove-marble)
                 p1-game-insert-marble)]
    (-> game
        (update :round inc)
        action
        (update :marbles-to-play rest)
        (update :player-turns rest))))

(defn p1-play-game [game]
  (loop [game game]
    (if (seq (:marbles-to-play game))
      (recur (p1-play-round game))
      game)))

(defn p1-solution []
  (->> (p1-game 468 71010)
       (p1-play-game)
       :scores
       (sort-by val)
       last
       time))

;; Day 9 - Part II unsolved
