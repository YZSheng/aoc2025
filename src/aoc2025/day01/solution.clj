(ns aoc2025.day01.solution
  (:require
   [clojure.string :as string]))


(def sample-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defn parse-input [input]
  (->> input
       (string/split-lines)
       (map #(let [[_ dir dist] (re-matches #"([LR])(\d+)" %)]
               {:dir dir
                :dist (Integer/parseInt dist)}))))

(parse-input sample-input)

(defn rotate [pos instruction]
  (let [{:keys [dir dist]} instruction
        turn (case dir
               "L" -1
               "R" 1)]
    (mod (+ pos (* turn dist)) 100)))

(rotate 50 {:dir "L" :dist 68})

(defn solve-part1 [input]
  (let [instructions (parse-input input)]
    (loop [zero-hit 0
           pos 50
           [inst & rest] instructions]
      (if inst
        (let [new-pos (rotate pos inst)]
          (if (= new-pos 0)
            (recur (inc zero-hit) new-pos rest)
            (recur zero-hit new-pos rest)))
        zero-hit))))

(solve-part1 sample-input)

(solve-part1 (slurp "resources/day01/input.txt"))