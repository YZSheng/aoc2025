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


(defn rotate [pos instruction]
  (let [{:keys [dir dist]} instruction
        turn (case dir
               "L" -1
               "R" 1)]
    (mod (+ pos (* turn dist)) 100)))


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

(defn rotate-2 [pos instruction]
  (let [{:keys [dir dist]} instruction
        turn (case dir
               "L" -1
               "R" 1)
        new-pos (+ pos (* turn dist))
        modded (mod new-pos 100)
        abs-quot (abs (quot new-pos 100))]
    (cond
      (= pos 0) [abs-quot modded]
      (> pos 0) (case turn
                  1 [abs-quot modded]
                  -1 (if (<= new-pos 0)
                       [(inc abs-quot) modded]
                       [0 modded]))
      (< pos 0) (case turn
                  1 (if (>= new-pos 0)
                      [(inc abs-quot) modded]
                      [0 modded])
                  -1 [abs-quot modded])
      :else (throw (Exception. (str "Unexpected pos: " pos))))))

(comment
  (parse-input sample-input)
  (rotate 50 {:dir "L" :dist 68})

  ;; 1 -> 102 => [1 2]
  (rotate-2 1 {:dir "R" :dist 101})
  ;; 99 -> 100 => [1 0]
  (rotate-2 99 {:dir "R" :dist 1})
  ;; 50 -> 1050 => [10 50]
  (rotate-2 50 {:dir "R" :dist 1000})
  ;; 0 -> 100 => [1 0]
  (rotate-2 0 {:dir "R" :dist 100})
  ;; 1 -> -1 => [1 99]
  (rotate-2 1 {:dir "L" :dist 2})
  ;; 50 -> 30 => [0 30]
  (rotate-2 50 {:dir "L" :dist 20})
  ;; 1 -> -101 => [2 99]
  (rotate-2 1 {:dir "L" :dist 102})

  ;; -1 -> -101 => [1 99]
  (rotate-2 -1 {:dir "L" :dist 100})
  ;; -1 -> -2 => [0 98]
  (rotate-2 -1 {:dir "L" :dist 1})
  ;; 0 -> -100 => [1 0]
  (rotate-2 0 {:dir "L" :dist 100})
  ;; -50 -> 50 => [1 50]
  (rotate-2 -50 {:dir "R" :dist 100})
  ;; -50 -> -40 => [0 60]
  (rotate-2 -50 {:dir "R" :dist 10})
  ;; 55 -> 0 => [1 0]
  (rotate-2 55 {:dir "L" :dist 55})
  ;; 99 -> 0 => [1 0]
  (rotate-2 99 {:dir "R" :dist 1})
  ;; -1 -> 0 => [1 0]
  (rotate-2 -1 {:dir "R" :dist 1}))

(defn solve-part2 [input]
  (let [instructions (parse-input input)]
    (loop [zero-hit 0
           pos 50
           [inst & rest] instructions]
      (if inst
        (let [[passed-zero new-pos] (rotate-2 pos inst)]
          (recur (+ zero-hit passed-zero) new-pos rest))
        zero-hit))))

(solve-part2 sample-input)

(solve-part2 (slurp "resources/day01/input.txt"))