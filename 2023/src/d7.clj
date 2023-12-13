(ns d7
  "Camel Cards"
  (:require [clojure.data :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def rank
  (into {} (map vec (partition 2 (interleave "AKQJT98765432" (range 14 0 -1))))))

(def rank-2
  (into {} (map vec (partition 2 (interleave "AKQT98765432J" (range 14 0 -1))))))

(defn parse-cards [s]
  (let [[hand bid] (str/split s #" ")]
    {:hand hand
     :bid (Integer/parseInt bid)}))

(defn read-game [f]
  (->> (slurp (io/resource f))
       (str/split-lines)
       (map parse-cards)))

(defn classify [hand]
  (vec (take 5 (concat (->> (frequencies hand) vals sort reverse)
                       (repeat 0)))))

(defn classify-2 [hand]
  (let [n (count (filter #{\J} hand))
        no-jokers (classify (remove #{\J} hand))]
    (vec (concat [(+ n (first no-jokers))] (rest no-jokers)))))

(defn enrich [cards rankf classifyf]
  (assoc cards
         :value (mapv rankf (:hand cards))
         :type (classifyf (:hand cards))))

(defn sort-game [game]
  (->> game
       (group-by :type)
       (sort-by first)
       (map (fn [[t m]] [t (sort-by :value m)]))
       (mapcat second)))

(defn score-game [game]
  (reduce + (map-indexed (fn [i m] (* (inc i) (:bid m))) game)))

(defn solve [game rankf classifyf]
  (->> game
       (map #(enrich % rankf classifyf))
       sort-game
       score-game))

(solve (read-game "d7_example.txt") rank classify)
(solve (read-game "d7.txt") rank classify)

(solve (read-game "d7_example.txt") rank-2 classify-2)
(solve (read-game "d7.txt") rank-2 classify-2)
