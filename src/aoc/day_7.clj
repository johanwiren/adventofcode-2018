(ns aoc.day-7
  (:require [clojure.java.io :as io]
            [weavejester.dependency :as dependency]))

(defn small-input []
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])

(defn input []
  (->> "aoc-day-7.txt"
       io/resource
       io/reader
       line-seq))

(defn p1-lexical-comparator [x y]
  (compare (name x) (name y)))

(defn p1-deps [input]
  (->> input
       (reduce (fn [acc row]
                 (let [[parent child]
                       (->> row
                            (re-find #"Step (\w) .* step (\w).*")
                            rest
                            (map keyword))]
                   (dependency/depend acc child parent)))
               (dependency/graph))))

(defn p1-sorter [graph]
  (loop [graph graph
         sorted []]
    (if-let [node (some->> (dependency/nodes graph)
                           (filter #(empty? (dependency/immediate-dependencies graph %)))
                           (sort p1-lexical-comparator)
                           first)]
      (recur (dependency/remove-all graph node) (conj sorted node))
      sorted)))

(defn p1-solution []
  (->> (p1-deps (input))
       p1-sorter
       (map name)
       (apply str)
       time))

(def p2-min-step-duration 60)
(def p2-num-workers 5)

(defn p2-task [task]
  {:task task
   :time-left (-> task name first int (- 64) (+ p2-min-step-duration))})

(defn p2-do-work [task seconds]
  (update task :time-left #(- % seconds)))

(defn p2-done? [worker]
  (zero? (get worker :time-left 0)))

(defn p2-available-tasks [graph tasks]
  (some->> (dependency/nodes graph)
           (filter #(empty? (dependency/immediate-dependencies graph %)))
           (remove (partial contains? (->> tasks (map :task) set)))
           (sort p1-lexical-comparator)
           not-empty
           (map p2-task)))

(defn p2-solver [input]
  (loop [graph (p1-deps input)
         time-elapsed 0
         tasks nil]
    (let [done-tasks (filter p2-done? tasks)
          graph (reduce (fn [graph task]
                          (dependency/remove-all graph task))
                        graph
                        (map :task done-tasks))
          available-tasks (p2-available-tasks graph tasks)
          new-tasks (->> (concat (remove p2-done? tasks) available-tasks)
                         (take p2-num-workers))
          tick (some->> new-tasks
                        not-empty
                        (map :time-left)
                        (apply min))]
      (if (seq new-tasks)
        (do
          (println new-tasks)
          (recur graph (+ time-elapsed tick) (map #(p2-do-work % tick) new-tasks)))
        time-elapsed))))

(defn p2-solution []
  (p2-solver (input)))
