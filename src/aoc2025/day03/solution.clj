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


(defn find-largest-n-digit-number [input n]
  (let [digits (str/split input #"")
        len (count digits)
        memo (atom {})]
    (if (> n len)
      0
      (letfn [(dp [idx remaining]
                (if-let [cached (@memo [idx remaining])]
                  cached
                  (let [result (cond
                                 (zero? remaining) ""
                                 (> remaining (- len idx)) ""
                                 (= idx len) ""
                                 :else (let [take (str (digits idx) (dp (inc idx) (dec remaining)))
                                             skip (dp (inc idx) remaining)
                                             take-num (if (empty? take) 0 (parse-long take))
                                             skip-num (if (empty? skip) 0 (parse-long skip))]
                                         (if (>= take-num skip-num) take skip)))]
                    (swap! memo assoc [idx remaining] result)
                    result)))]
        (parse-long (dp 0 n))))))

(find-largest-n-digit-number "987654321111111" 12)
(find-largest-n-digit-number "811111111111119" 12)
(find-largest-n-digit-number "234234234234278" 12)
(find-largest-n-digit-number "818181911112111" 12)
(find-largest-n-digit-number "2213322222223222132231233322432224423222226232323522232215252332221122222231232224232722131522422232" 12)

(defn solve-2 [input]
  (->> (str/split-lines input)
       (map #(find-largest-n-digit-number % 12))
       (reduce +)))

(solve-2 sample-input)
(solve-2 (slurp "resources/day03/input.txt"))