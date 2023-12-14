(ns d9
  "Mirage Maintenance"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example [[0 3 6 9 12 15]
              [1 3 6 10 15 21]
              [10 13 16 21 30 45]])

(defn parse-int [s] (Integer/parseInt s))

(defn parse-line [s]
  (map parse-int (str/split s #"\s+")))

(defn read-report [f]
  (map parse-line (str/split-lines (slurp (io/resource f)))))

(defn derive [coll]
  (map - (rest coll) coll))

(defn all-zeros [coll]
  (every? zero? coll))

(defn history [coll]
  (reverse (take-while (complement all-zeros)
                       (iterate derive (seq coll)))))

(defn extrapolate [hist op rev]
  (loop [seed [0] hist (map rev hist)]
    (if (not (seq hist))
      seed
      (recur (conj (first hist) (op (ffirst hist) (first seed)))
             (rest hist)))))

(def report (read-report "d9.txt"))

;; (println (reduce + (map first (map #(extrapolate (history %) + reverse) example))))
;; (println (reduce + (map first (map #(extrapolate (history %) - identity) example))))

(println "part 1:" (reduce + (map first (map #(extrapolate (history %) + reverse) report))))
(println "part 2:" (reduce + (map first (map #(extrapolate (history %) - identity) report))))
