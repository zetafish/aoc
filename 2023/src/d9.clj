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

(defn next-number [lower upper]
  (+ (last lower) (last upper)))

(defn extrapolate-1 [lower upper]
  (conj upper (+ (first upper) (first lower))))

(defn extrapolate [hist]
  (loop [seed [0] hist (map reverse hist)]
    ;; (println seed)
    (if (not (seq hist))
      seed
      (recur (extrapolate-1 seed (first hist))
             (rest hist)))))

(def report (read-report "d9.txt"))

(println (reduce + (map first (map #(extrapolate (history %)) report))))
