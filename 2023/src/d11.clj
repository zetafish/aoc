(ns d11
  "Cosmic Expansion"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [medley.core :as m]))

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

(defn empty-row? [grid n]
  (every? #{\.} (nth grid n)))

(defn empty-col? [grid n]
  (every? #{\.} (map #(nth % n) grid)))

(defn empty-rows [grid]
  (for [n (range (count grid))
        :when (empty-row? grid n)] n))

(defn empty-cols [grid]
  (for [n (range (count (first grid)))
        :when (empty-col? grid n)] n))

(defn path [[ya xa] [yb xb]]
  {:y-range (if (< ya yb) [ya yb] [yb ya])
   :x-range (if (< xa xb) [xa xb] [xb xa])})

(defn axial-dist [[a b] dups n]
  (+ (- b a)
     (* (dec n) (count (filter #(<= a % b) dups)))))

(defn find-galaxies [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= \# (get-in grid [y x]))]
    [y x]))

(defn galaxy-pairs [grid]
  (let [coll (find-galaxies grid)]
    (->> (for [p coll q coll :when (not= p q)] [p q])
         (map sort)
         distinct)))

(defn solve [n grid]
  (let [pairs (galaxy-pairs grid)
        paths (map (partial apply path) pairs)
        x-ranges (frequencies (map :x-range paths))
        y-ranges (frequencies (map :y-range paths))
        x-empty (set (empty-cols grid))
        y-empty (set (empty-rows grid))]
    (+ (reduce + (vals (m/map-kv-vals (fn [k v] (* v (axial-dist k x-empty n))) x-ranges)))
       (reduce + (vals (m/map-kv-vals (fn [k v] (* v (axial-dist k y-empty n))) y-ranges))))))

(comment
  (println (solve 2 example1))
  (println (solve 2 data))
  (println (solve 100 example1))
  (println (solve 1000000 data)))

