(ns aoc2025.day03.solution 
  (:require
   [clojure.string :as str]))

(def sample-input "987654321111111
811111111111119
234234234234278
818181911112111
")

(defn find-largest-number [input]
  (let [chars (str/split input #"")
        digits (map parse-long chars)]
    (loop [result 0
           [d & rest] digits]
      (if (and d (not (empty? rest)))
        (let [remaining-max (apply max rest)
              new-result (parse-long (str d remaining-max))]
          (recur (max result new-result) rest))
        result))))


(find-largest-number "987654321111111")
(find-largest-number "811111111111119")
(find-largest-number "234234234234278")
(find-largest-number "818181911112111")

(defn solve-1 [input]
  (->> (str/split-lines input)
       (map find-largest-number)
       (reduce +)))

(solve-1 sample-input)
(solve-1 (slurp "resources/day03/input.txt"))