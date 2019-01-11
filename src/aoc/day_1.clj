(ns aoc.day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn input []
  (-> "aoc-day-1.txt"
      io/resource
      io/reader
      slurp
      str/trim-newline))

(defn p1-solution []
  (reduce + (map clojure.edn/read-string (str/split (input) #" "))))

(defn p2-solution []
  (let [changes (->> (str/split (input) #" ")
                     (map clojure.edn/read-string))]
    (loop [f 0
           fs #{}
           [change & more] (flatten (repeat changes))]
      (if (fs f)
        f
        (recur (+ f change) (conj fs f) more)))))
