(ns aoc.day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (com.google.common.hash BloomFilter
                                   Funnels)))

(defn input []
  (-> "aoc-day-1.txt"
      io/resource
      io/reader
      slurp
      str/trim-newline))

(defn p1-solution []
  (reduce +
          (map #(Integer/parseInt %) (str/split (input) #" "))))

(defn p2-solution-list-and-bloom-impl []
  (let [changes (map #(Long/parseLong %)
                     (str/split (input) #" "))
        bloom-filter (BloomFilter/create (Funnels/longFunnel) 1000000)]
    (reduce (fn [fs change]
              (let [f (+ (first fs) change)]
                (if (.mightContain bloom-filter f)
                  (do
                    (println "Entry might be in bloom filter: " f)
                    (when (some #{f} fs)
                      (do
                        (println "Solution found: " f)
                        (reduced f))))
                  (do
                    (.put bloom-filter f)
                    (cons f fs)))))
            '(0)
            (flatten (repeat changes)))))

(defn p2-solution []
  (p2-solution-list-and-bloom-impl))
