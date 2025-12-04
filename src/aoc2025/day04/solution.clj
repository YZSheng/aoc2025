(ns aoc2025.day04.solution 
  (:require
   [clojure.string :as str]))

(def sample-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
")

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(str/split % #""))))

(get-in (parse-input sample-input) [0 0])

(defn get-neighbors [grid row col]
  (let [rows (count grid)
        cols (count (first grid))
        deltas [[-1 -1] [-1 0] [-1 1]
                [0 -1]         [0 1]
                [1 -1]  [1 0]  [1 1]]]
    (for [[dr dc] deltas
          :let [new-r (+ row dr)
                new-c (+ col dc)
                new-value (get-in grid [new-r new-c])]
          :when (and (>= new-r 0)
                     (< new-r rows)
                     (>= new-c 0)
                     (< new-c cols)
                     (= new-value "@"))]
      {:row new-r
       :col new-c
       :value new-value})))

(get-neighbors (parse-input sample-input) 0 0)

(defn solve-1 [input]
  (->> (let [grid (parse-input input)
             rows (count grid)
             cols (count (first grid))]
         (for [row (range rows)
               col (range cols)
               :when (= (get-in grid [row col]) "@")]
           (get-neighbors grid row col)))
       (filter #(< (count %) 4))
       count))

(solve-1 sample-input)
(solve-1 (slurp "resources/day04/input.txt"))

(defn one-pass [grid]
  (->> (let [rows (count grid)
             cols (count (first grid))]
         (for [row (range rows)
               col (range cols)
               :when (= (get-in grid [row col]) "@")]
           [row col (get-neighbors grid row col)]))
       (filter #(< (count (last %)) 4))
       (reduce (fn [[counter grid] [row col _]]
                 (let [new-grid (assoc-in grid [row col] ".")]
                   [(inc counter) new-grid]))
               [0 grid])))

(one-pass (parse-input sample-input))

(defn solve-2 [input]
  (loop [grid (parse-input input)
         total-removed 0]
    (let [[removed new-grid] (one-pass grid)]
      (if (zero? removed)
        total-removed
        (recur new-grid (+ total-removed removed))))))

(solve-2 sample-input)
(solve-2 (slurp "resources/day04/input.txt"))