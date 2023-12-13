(ns d7
  "Camel Cards"
  (:require [clojure.data :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def card-value
  (into {} (map vec (partition 2 (interleave "AKQJT98765432" (range 14 0 -1))))))

(defn parse-cards [s]
  (let [[hand bid] (str/split s #" ")
        cards (mapv card-value hand)
        type (vec (take 5
                        (concat (reverse (sort (vals (frequencies cards))))
                                (repeat 0))))]
    {:hand hand
     :type type
     :cards cards
     :bid (Integer/parseInt bid)}))

(defn read-game [f]
  (->> (str/split-lines (slurp (io/resource f)))
       (map parse-cards)))

(defn sort-game [game]
  (->> game
       (group-by :type)
       (sort-by first)
       (map (fn [[t m]] [t (sort-by :cards m)]))
       (mapcat second)))

(defn score-game [game]
  (reduce + (map-indexed (fn [i m] (* (inc i) (:bid m))) game)))

(println (score-game (sort-game (read-game "d7_example.txt"))))
(println (score-game (sort-game (read-game "d7.txt"))))

