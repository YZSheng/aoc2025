(ns aoc2025.day10.solution
  (:require
   [clojure.string :as str]))


(def sample-line "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

(defn parse-target [target]
  (mapv (fn [c] (if (= c \.) false true)) target))


(defn parse-line [line]
  (let [[_ target buttons constraints] (re-matches #"\[(.*)\] (\(.*\)) \{(.*)\}" line)]
    {:target (parse-target target) :buttons (->> (str/split buttons #"\s+")
                                                 (map #(-> %
                                                           (str/replace #"[\(\)]" "")
                                                           (str/split #",")))
                                                 (map (fn [s] (mapv #(Integer/parseInt %) s)))) :constraints constraints}))
(defn combinations [coll size]
  (cond
    (= size 0) '(())
    (empty? coll) '()
    :else (concat (map (fn [x] (cons (first coll) x))
                       (combinations (rest coll) (dec size)))
                  (combinations (rest coll) size))))

(parse-line sample-line)

(defn press-buttons [state buttons]
  (reduce (fn [acc btn]
            (update acc btn #(not %)))
          state
          buttons))

(press-buttons [false true true false] [1 3])

(defn find-min-presses [target buttons]
  (let [initial-state (vec (repeat (count target) false))]
    (loop [queue [[initial-state 0]]  ; [state, presses-count]
           visited #{initial-state}
           max-presses 20]
      (if (empty? queue)
        (throw (Exception. "No solution found"))
        (let [[state curr-presses] (first queue)
              rest-queue (rest queue)]
          (cond
            (= state target)
            curr-presses

            (>= curr-presses max-presses)
            (recur rest-queue visited max-presses)

            :else
            (let [next-presses (inc curr-presses)
                  new-combinations (combinations buttons next-presses)
                  new-states (doall
                              (->> new-combinations
                                   (map (fn [comb]
                                          (press-buttons initial-state (apply concat comb))))
                                   (filter #(not (visited %)))
                                   (map #(vector % next-presses))))]
              (recur (concat rest-queue new-states)
                     (into visited (map first new-states))
                     max-presses))))))))

(defn min-presses [line]
  (let [{:keys [target buttons constraints]} (parse-line line)]
    (find-min-presses target buttons)))

(min-presses sample-line)

(defn solve-1 [input-lines]
  (->> input-lines
       (map min-presses)
       (reduce +)))

(solve-1 (str/split-lines (slurp "resources/day10/input.txt")))
