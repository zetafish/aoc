(ns d5
  "If You Give A Seed A Fertilizer"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s] (Integer/parseInt s))

(defn parse-seeds [[seeds]]
  (map parse-int
       (str/split
        (second (str/split seeds #"seeds: "))
        #" +")))

(defn parse-number-grid [lines]
  (map (comp (partial map parse-int)
             #(str/split % #" +")) lines))

(def params [:seed
             :soil
             :fertilizer
             :water
             :light
             :temperature
             :humidity
             :location])

(def transitions (partition 2 (interleave params (rest params))))

(defn read-almanac [f]
  (let [sections (->> (str/split-lines (slurp (io/resource f)))
                      (partition-by #{""})
                      (remove #{[""]})
                      vec)]
    (assoc (zipmap transitions
                   (map (comp parse-number-grid rest) (rest sections)))
           :seeds (parse-seeds (sections 0)))))

(defn apply-almanac [almanac plant from to]
  (if-let [n (->> (get almanac [from to])
                  (filter (fn [[_ src n]]
                            (<= src (from plant) (+ src n -1))))
                  (map (fn [[dst src _]] (+ (from plant) (- src) dst)))
                  first)]
    (assoc plant to n)
    (assoc plant to (from plant))))

(def almanac (read-almanac "d5_small.txt"))

(reduce (fn [plant [from to]]
          (apply-almanac almanac plant from to))
        {:seed 79}
        transitions)

