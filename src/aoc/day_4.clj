(ns aoc.day-4
  (:require [clojure.java.io :as io]))

(def p1-input (->> "aoc-day-4.txt"
                      io/resource
                      io/reader
                      line-seq
                      sort))

(defn solver [input sort-key]
  (let [log (loop [lines input
                   log {}
                   guard nil]
              (if (nil? lines)
                log
                (let [line (first lines)
                      new-guard (or (last (re-matches #".*(?:Guard #)(\d+)? .*" line))
                                    guard)
                      log-minutes (second (re-matches #".*:(\d\d).*(falls|wakes).*" line))]
                  (recur (next lines)
                         (if log-minutes
                           (update log new-guard conj (Integer/parseInt log-minutes))
                           log)
                         new-guard))))]
    (->> log
         (map (fn [[guard log-minutes]]
                (let [minutes-slept (->> log-minutes
                                         reverse
                                         (partition 2)
                                         (map (partial apply range))
                                         flatten)
                      [best-minute times-slept] (->> minutes-slept
                                                     frequencies
                                                     (sort-by second)
                                                     last)]
                  {:guard (Integer/parseInt guard)
                   :best-minute best-minute
                   :best-minute-count times-slept
                   :minutes-slept (count minutes-slept)})))
         (sort-by sort-key)
         last)))

(def p1-solution (->> (solver p1-input :minutes-slept)
                         ((juxt :guard :best-minute))
                         (apply *)))

(def p2-solution (->> (solver p1-input :best-minute-count)
                         ((juxt :guard :best-minute))
                         (apply *)))
