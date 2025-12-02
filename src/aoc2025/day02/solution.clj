(ns aoc2025.day02.solution
  (:require
   [clojure.string :as str]))


(def sample-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn valid-id? [id-num]
  (let [id (str id-num)
        left-half (subs id 0 (quot (count id) 2))
        right-half (subs id (quot (count id) 2))]
    (= left-half right-half)))

(valid-id? 123123)
(valid-id? 123321)
(valid-id? 99)
(valid-id? 1188511885)

(defn parse-input [input]
  (->> (str/split input #",")
       (map (fn [s]
              (str/split s #"-")))
       (map (fn [[start end]]
              (range (parse-long start)
               (inc (parse-long end)))))
       (mapcat (fn [nums]
              (filter valid-id? nums)))
       (reduce +)))

(parse-input sample-input)

(parse-input (slurp "resources/day02/input.txt"))
