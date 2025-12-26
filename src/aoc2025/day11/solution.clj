(ns aoc2025.day11.solution 
  (:require
   [clojure.string :as str]))

(def sample-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #": "))
       (map (fn [[k v]] [k (str/split v #" ")]))
       (into {})))

(parse-input sample-input)

(defn find-non-recursive-paths [graph start end visited]
  (if (= start end)
    [[end]]
    (if (visited start)
      []
      (let [new-visited (conj visited start)]
        (apply concat
               (for [neighbor (get graph start [])]
                 (let [paths (find-non-recursive-paths graph neighbor end new-visited)]
                   (map #(cons start %) paths))))))))

(defn solve-1 [input]
  (let [graph (parse-input input)
        paths (find-non-recursive-paths graph "you" "out" #{})]
    (count paths)))

(solve-1 sample-input)

(solve-1 (slurp "resources/day11/input.txt"))