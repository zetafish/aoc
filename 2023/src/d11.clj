(ns d11
  "Cosmic Expansion"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example1 (mapv vec ["...#......"
                         ".......#.."
                         "#........."
                         ".........."
                         "......#..."
                         ".#........"
                         ".........#"
                         ".........."
                         ".......#.."
                         "#...#....."]))

(def data (mapv vec (str/split-lines (slurp (io/resource "d11.txt")))))

(defn dump [universe]
  (println (str/join "\n" (map str/join universe))))

(split-at 5 [0 1 2 3 4 5 6 7 8])

(defn duplicate-nth [grid n]
  (let [[l r] (split-at n grid)]
    (concat l [(nth grid n)] r)))

(defn duplicate-row [grid n]
  (duplicate-nth grid n))

(defn duplicate-col [grid n]
  (map #(duplicate-nth % n) grid))

(defn empty-row? [grid n]
  (every? #{\.} (nth grid n)))

(defn empty-col? [grid n]
  (every? #{\.} (map #(nth % n) grid)))

(defn expand-rows [grid]
  (->> (for [n (range (count grid))
             :when (empty-row? grid n)] n)
       reverse
       (reduce duplicate-row grid)
       (mapv vec)))

(defn expand-cols [grid]
  (->> (for [n (range (count (first grid)))
             :when (empty-col grid n)] n)
       reverse
       (reduce duplicate-col grid)
       (mapv vec)))

(defn dist [p q]
  (reduce + (map abs (map - q p))))

(defn find-galaxies [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= \# (get-in grid [y x]))]
    [y x]))

(defn sum-shortest-paths [grid]
  (let [coll (find-galaxies grid)]
    (->> (for [p coll q coll :when (not= p q)] [p q])
         (map sort)
         distinct
         (map (fn [[p q]] (dist p q)))
         (reduce +))))

(defn solve-1 [grid]
  (->> grid
       expand-rows
       expand-cols
       sum-shortest-paths))

(solve-1 example1)
(solve-1 data)

