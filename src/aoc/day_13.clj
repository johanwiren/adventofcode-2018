(ns aoc.day-13
  (:require [clojure.java.io :as io]
            [lanterna.terminal :as terminal]))

(defn enumerate [seq]
  (map-indexed (fn [i item] [i item]) seq))

(def input-to-track
  {\< \-
   \> \-
   \^ \|
   \v \|})

(def to-direction
  {\< :left
   \> :right
   \^ :up
   \v :down})

(def turn-left
  {:left :down
   :right :up
   :up :left
   :down :right})

(def turn-right
  {:left :up
   :right :down
   :up :right
   :down :left})

(defn build-cart [input-char x y]
  (when ((-> input-to-track keys set) input-char)
    {:direction (to-direction input-char)
     :position [x y]
     :xing-actions (flatten (repeat [turn-left identity turn-right]))}))

(defn parser [lines]
  (reduce
   (fn [[tracks carts] [y line]]
     (reduce
      (fn [[tracks-acc carts-acc] [x input-char]]
        (let [tracks (if (not (= \space input-char))
                       (assoc tracks-acc
                              [x y]
                              (get input-to-track input-char input-char))
                       tracks-acc)
              carts (if (contains? (-> to-direction keys set) input-char)
                      (conj carts-acc (build-cart input-char x y))
                      carts-acc)]
          [tracks carts]))
      [tracks carts]
      (enumerate line)))
   [{} []]
   (enumerate lines)))

(defn input [size]
  (-> (format "aoc-day-13-%s.txt" (name size))
      io/resource
      io/reader
      line-seq
      parser))

(defn p1-handle-xing [{:keys [xing-actions] :as cart} map-char]
  (if (= \+ map-char)
    (-> cart
        (update :direction (first xing-actions))
        (update :xing-actions rest))
    cart))

(def p1-turns
  {[\\ :up] :left
   [\\ :down] :right
   [\\ :left] :up
   [\\ :right] :down
   [\/ :up] :right
   [\/ :down] :left
   [\/ :left] :down
   [\/ :right] :up})

(defn p1-handle-turn [{:keys [direction] :as cart} map-char]
  (if (#{\\ \/} map-char)
    (let [new-direction (get p1-turns [map-char direction])]
      (assoc cart :direction new-direction))
    cart))

(defn move-cart [{:keys [direction position actions] :as cart} tracks]
  (let [[x y] position
        new-pos (case direction
                  :up [x (dec y)]
                  :down [x (inc y)]
                  :left [(dec x) y]
                  :right [(inc x) y])
        map-char (get tracks new-pos)]
    (assert map-char)
    (-> cart
        (assoc :position new-pos)
        (p1-handle-xing map-char)
        (p1-handle-turn map-char))))

(defn collisions [carts]
  (some->> carts
           (group-by :position)
           (filter #(-> %
                        val
                        count
                        (> 1)))
           seq
           keys))

(defn tick [carts tracks]
  (let [carts (sort-by :position carts)]
    (loop [moved-carts []
           [cart-to-move & more] carts]
      (let [moved-carts (conj moved-carts (move-cart cart-to-move tracks))
            collisions (collisions (concat moved-carts more))]
        (assert (not collisions) collisions)
        (if more
          (recur moved-carts more)
          moved-carts)))))

(def draw-direction
  {:down \v
   :up \^
   :left \<
   :right \>})

(defn draw [term tracks carts]
  (terminal/clear term)
  (run! (fn [[[x y] char]]
          (terminal/move-cursor term x y)
          (terminal/put-character term char))
        tracks)
  (run! (fn [{:keys [position direction]}]
          (apply (partial terminal/move-cursor term) position)
          (terminal/put-character term (draw-direction direction)))
        carts))

(defn p1-solver []
  (let [[tracks carts] (input :large)]
    (loop [carts carts]
      (recur (tick carts tracks)))))

(defn p2-tick [carts tracks]
  (let [carts (sort-by :position carts)]
    (loop [moved-carts []
           [cart-to-move & more] carts]
      (let [moved-carts (conj moved-carts (move-cart cart-to-move tracks))
            collisions (collisions (concat moved-carts more))
            remove-fn (comp (set collisions) :position)
            not-collided-more (->> more
                                   (remove remove-fn)
                                   seq)]
        (if not-collided-more
          (recur (remove remove-fn moved-carts) not-collided-more)
          (remove remove-fn moved-carts))))))

(defn p2-solver []
  (let [[tracks carts] (input :large)]
    (loop [carts carts]
      (if (< 1 (count carts))
        (recur (p2-tick carts tracks))
        (-> carts
            first
            :position)))))
