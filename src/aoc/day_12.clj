(ns aoc.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn small-input []
  (-> "aoc-day-12-small.txt"
      io/resource
      io/reader
      line-seq))

(defn input []
  (-> "aoc-day-12.txt"
      io/resource
      io/reader
      line-seq))

(defn parse-input [input]
  (let [[init-line notes]
        (->> input
             (remove #{""})
             (split-at 1))]
    [{:pots (-> init-line
                first
                (str/split #" ")
                last)
      :leftmost-pot 0}
     (->> notes
          (map (fn [note]
                 (let [[pattern result] (str/split note #" => ")]
                   [pattern result])))
          (into {}))]))

(defn p1-apply-notes [pots notes]
  (get notes pots "."))

(defn p1-sliding-window [pots]
  (let [pots (str "...." pots "....")]
    (for [i (range (- (count pots) 4))]
      (subs pots i (+ i 5)))))

(defn print-pots [pots]
  (let [pre (count (first (re-seq #"^\.+" pots)))
        post (count (first (re-seq #"\.+$" pots)))
        printable-pots (-> pots
                           (str/replace #"^\.+[^#]" "")
                           (str/replace #"[^#]\.+$" ""))]
    (println (format "{%s}%s{%s}" pre printable-pots post))))

(defn p1-generate [gen notes num-gens]
  (reduce (fn [{:keys [pots] :as gen} _]
            (let [new-pots
                  (reduce (fn [new-pots window]
                            (str new-pots (p1-apply-notes window notes)))
                          ""
                          (p1-sliding-window pots))]
              (print-pots new-pots)
              (-> gen
                  (assoc :pots new-pots)
                  (update :leftmost-pot #(- % 2)))))
          gen
          (range num-gens)))

(defn p1-get-sum
  ([]
   (p1-get-sum 20))
  ([generations]
   (let [[gen notes] (-> (input)
                         parse-input)
         res (time (p1-generate gen notes generations))]
     (->> (map-indexed (fn [i item]
                         [(+ (:leftmost-pot res) i) item])
                       (:pots res))
          (filter (comp #{\#} last))
          (map first)
          (apply +)))))

(defn p2-solution []
  ;; Noticed that the pattern started repeating after about 150
  (let [res1 (p1-get-sum 200)
        res2 (p1-get-sum 201)]
    (+ res1
       (* (- 50000000000 200)
          (- res2 res1)))))
