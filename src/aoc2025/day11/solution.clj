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


(def sample-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")


(defn count-paths-through
  ([graph start end must-visit]
   (count-paths-through graph start end must-visit #{} (atom {})))
  ([graph start end must-visit visited memo]
   (let [memo-key [start end must-visit]]
     (if (contains? @memo memo-key)
       (@memo memo-key)
       (let [result (if (= start end)
                      (if (empty? must-visit) 1 0)
                      (if (visited start)
                        0
                        (let [new-visited (conj visited start)
                              new-must-visit (disj must-visit start)
                              neighbors (get graph start [])]
                          (reduce + 0 (for [neighbor neighbors]
                                        (count-paths-through graph neighbor end new-must-visit new-visited memo))))))]
         (swap! memo assoc memo-key result)
         result)))))

(defn solve-2 [input]
  (let [graph (parse-input input)]
    (count-paths-through graph "svr" "out" #{"fft" "dac"})))

(solve-2 sample-input-2)
(solve-2 (slurp "resources/day11/input.txt"))