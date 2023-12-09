(ns d2
  "Cube Conundrum"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def re-game #"Game \d+: \d+ \w")

(defn parse-line [s]
  (let [[game pins] (str/split s #":")
        pins (map (comp #(re-matches #"\d+ \w+" %) str/trim #(str/split % #",")) pins)]
    pins))

(defn parse-game [s]
  (let [[game cubes] (str/split s #":")
        id (Integer/parseInt (last (str/split game #" ")))]
    {:id id
     :reveals (->> (str/split cubes #";")
                   (map (comp
                         #(into {} %)
                         (partial map (comp
                                       (fn [[d w]] [(keyword w) (Integer/parseInt d)])
                                       rest #(re-matches #"(\d+) (\w+)" %) str/trim))
                         #(str/split % #",")
                         str/trim)))}))

(defn check-reveal [reveal bag]
  (and (<= (:red reveal 0) (:red bag 0))
       (<= (:green reveal 0) (:green bag 0))
       (<= (:blue reveal 0) (:blue bag 0))))

(defn check-game [game bag]
  (every? #(check-reveal % bag) (:reveals game)))

(defn game-needs [{:keys [reveals]}]
  {:red (apply max (keep :red reveals))
   :green (apply max (keep :green reveals))
   :blue (apply max (keep :blue reveals))})

(defn parse-input [r]
  (->> (slurp (io/resource r))
       str/split-lines
       (map parse-game)))

(def bag {:red 12 :green 13 :blue 14})

(->> (parse-input "d2.txt")
     (map #(assoc % :valid? (check-game % bag)))
     (filter :valid?)
     (map :id)
     (reduce +))

(->> (parse-input "d2.txt")
     (map #(game-needs %))
     (map (comp (partial reduce *) vals))
     (reduce +))

