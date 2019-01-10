(ns aoc.day-8
  (:require [clojure.java.io :as io]))

(def p1-small-input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
(def p1-input (-> "aoc-day-8.txt"
                  io/resource
                  slurp))

(defn p1-parse-input [input-str]
  (->> input-str
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn p1-reducer [child-nodes node-data build-node-fn]
  (reduce (fn [[children-meta more-data] _]
            (let [[child-meta more-data] (build-node-fn more-data)]
              [(concat children-meta child-meta) more-data]))
          [[] node-data]
          (range child-nodes)))

(defn p1-build-node [[child-nodes md-length & more-data]]
  (let [[children-meta more-data]
        (p1-reducer child-nodes more-data p1-build-node)]
    [(concat children-meta (take md-length more-data)) (drop md-length more-data)]))

(defn p2-build-node [[child-nodes md-length & more-data]]
  (let [[children-values more-data]
        (p1-reducer child-nodes more-data p2-build-node)]
    (let [[meta-data more-data] (split-at md-length more-data)
          value (if (seq children-values)
                  (->> meta-data
                       (map #(nth children-values (dec %) 0))
                       (apply +))
                  (apply + meta-data))]
      [[value] more-data])))

(def p1-solution
  (delay (->> p1-input
              p1-parse-input
              p1-build-node
              first
              (apply +))))

(def p2-solution
  (delay (->> p1-input
              p1-parse-input
              p2-build-node
              first)))
