(ns d8
  "Haunted Wasteland"
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [lcm]]
            [clojure.string :as str]))

(defn parse-dirs [s]
  (mapv {\L 0 \R 1} s))

(defn parse-nodes [s]
  (let [[_ a b c] (re-matches #"(\w+) = \((\w+), (\w+)\)" s)]
    [a [b c]]))

(defn read-network [f]
  (let [coll (str/split-lines (slurp (io/resource f)))
        graph (map parse-nodes (drop 2 coll))
        labels (sort (distinct (concat (map first graph)
                                       (mapcat second graph))))
        label->num (zipmap labels (iterate inc 0))
        num->label (zipmap (iterate inc 0) labels)]
    {:dirs (parse-dirs (first coll))
     :labels labels
     :label->num label->num
     :num->label num->label
     :graph (->> graph
                 (sort-by first)
                 (map second)
                 (mapv (partial mapv label->num)))}))

(defn select-nodes [network suffix]
  (->> (:labels network)
       (filter #(str/ends-with? % suffix))
       (map (:label->num network))))

(defn count-the-steps [{:keys [graph dirs] :as network} from to]
  (let [next-node (fn [node dir] (get-in graph [node dir]))
        dir (fn [counter] (get dirs (mod counter (count dirs))))]
    (loop [counter 0 from from]
      (cond
        (contains? to from) counter
        :else (recur (inc counter)
                     (next-node from (dir counter)))))))

(defn solve [network from-suffix to-suffix]
  (let [from (select-nodes network from-suffix)
        to (set (select-nodes network to-suffix))
        steps (map #(count-the-steps network % to) from)]
    (reduce lcm (first steps) (rest steps))))

(println (solve (read-network "d8.txt") "AAA" "ZZZ"))
(println (solve (read-network "d8.txt") "A" "Z"))
