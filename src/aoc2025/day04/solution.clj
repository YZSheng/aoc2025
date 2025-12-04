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