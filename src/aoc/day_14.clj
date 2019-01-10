(ns aoc.day-14
  (:require [clojure.string :as str]))

(def start
  {:generation 1
   :scores [3 7]
   :active-recipies [0 1]
   :last-scores "37"})

(defn to-digits [sum]
  (if (< 9 sum)
    [1 (rem sum 10)]
    [sum]))

(defn generate [{:keys [scores active-recipies] :as scoreboard}]
  (let [new-recipies (->> active-recipies
                          (map (fn [pos]
                                 (get scores pos)))
                          (apply +)
                          to-digits)
        new-scores (apply (partial conj scores) new-recipies)
        new-active (->> active-recipies
                        (map (fn [pos]
                               (let [steps (inc (get new-scores pos))]
                                 (if-let [active-recipe (get new-scores (+ pos steps))]
                                   (+ pos steps)
                                   (rem (+ pos steps) (count new-scores)))))))]
    (-> scoreboard
        (assoc :scores new-scores)
        (assoc :active-recipies new-active)
        (update :generation inc)
        (update :last-scores #(apply str (take-last 8 (str % (apply str new-recipies))))))))

(defn solve [input]
  (loop [scoreboard start]
    (if (< (count (:scores scoreboard)) (+ input 10))
      (recur (generate scoreboard))
      (->> scoreboard
           :scores
           (drop input)
           (take 10)
           (apply str)))))

(defn p1-solution []
  (solve 47801))

(defn p2-solve [input]
  (let [re (re-pattern input)
        iterate-fn (fn [{:keys [generation] :as scoreboard}]
                     (if (zero? (rem generation 100000))
                       (do
                         (println "Generation" generation)
                         (time (generate scoreboard)))
                       (generate scoreboard)))]
    (loop [scoreboard start]
      (let [score-str (:last-scores scoreboard)]
        (if (re-seq re score-str)
          (-> (str/split (->> scoreboard
                              :scores
                              (apply str))
                         re)
              first
              count)
          (recur (iterate-fn scoreboard)))))))

(defn p2-solution []
  (p2-solve "047801"))
