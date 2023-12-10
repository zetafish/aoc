(ns d4
  "Scratchcards"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn pow [x n]
  (reduce * (repeat n x)))

(defn parse-card [s]
  (let [[card numbers] (str/split s #": +")
        id (parse-int (second (re-matches #"Card +(\d+)" card)))
        [winners haves] (str/split numbers #" +\| +")
        winners (map parse-int (str/split winners #" +"))
        haves (map parse-int (str/split haves #" +"))
        matches (count (filter (set winners) haves))]
    [id {:matches matches
         :score (if (zero? matches) 0 (pow 2 (dec matches)))}]))

(defn read-cards [f]
  (into {} (map parse-card (str/split-lines (slurp (io/resource f))))))

(defn play-round [cards {:keys [todo pile]}]
  (if (empty? todo)
    {:pile pile}
    {:pile (+ pile (reduce + (map second todo)))
     :todo (->> todo
                (mapcat (fn [[id n]]
                          (->> (range (:matches (cards id)))
                               (map #(+ 1 id %))
                               (map #(vector % n)))))
                (sort-by first)
                (partition-by first)
                (map (fn [coll] [(ffirst coll) (reduce + (map second coll))])))}))

(defn scratch-till-you-drop [cards]
  (let [state {:pile 0 :todo (frequencies (keys cards))}]
    (->> (iterate (partial play-round cards) state)
         (drop-while (comp seq :todo))
         first)))

(reduce + (map :score (vals (read-cards "d4_small.txt"))))
(reduce + (map :score (vals (read-cards "d4.txt"))))

(scratch-till-you-drop (read-cards "d4_small.txt"))
(scratch-till-you-drop (read-cards "d4.txt"))

