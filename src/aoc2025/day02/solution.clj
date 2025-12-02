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

(defn solve-1 [input]
  (->> (str/split input #",")
       (map (fn [s]
              (str/split s #"-")))
       (map (fn [[start end]]
              (range (parse-long start)
                     (inc (parse-long end)))))
       (mapcat (fn [nums]
                 (filter valid-id? nums)))
       (reduce +)))

(solve-1 sample-input)

(solve-1 (slurp "resources/day02/input.txt"))

(partition 3 "121212")

(defn valid-id-2? [id-num]
  (let [id (str id-num)]
    (loop [partition-count 1
           found-repeat false]
      (cond
        found-repeat true
        (> partition-count (count id)) false
        (> (mod (count id) partition-count) 0) (recur (inc partition-count) found-repeat)
        :else (let [parts (partition partition-count id)
                    first-part (first parts)
                    all-same? (every? #(= first-part %) parts)]
                (recur (inc partition-count)
                       (and (> (count parts) 1) all-same?)))))))

(valid-id-2? 123123)
(valid-id-2? 1212)
(valid-id-2? 12121212)
;; false
(valid-id-2? 13)
(valid-id-2? 2121212121)
;; false
(valid-id-2? 2121212122)
(valid-id-2? 1188511885)
(valid-id-2? 11)


(defn solve-2 [input]
  (->> (str/split input #",")
       (map (fn [s]
              (str/split s #"-")))
       (map (fn [[start end]]
              (range (parse-long start)
                     (inc (parse-long end)))))
       (mapcat (fn [nums]
                 (filter valid-id-2? nums)))
       (reduce +)))

(solve-2 sample-input)
(solve-2 (slurp "resources/day02/input.txt"))
