(ns d4
  "Scratchcards"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn pow [x n]
  (reduce * (repeat n x)))

(defn parse-card [s]
  (let [[card numbers] (str/split s #": +")
        id (parse-int (second (re-matches #"Card +(\d+)" card)))
        [winners haves] (str/split numbers #" +\| +")
        winners (map parse-int (str/split winners #" +"))
        haves (map parse-int (str/split haves #" +"))]
    {:id id
     :winners (set winners)
     :haves (set haves)
     :score (let [n (count (filter (set winners) haves))]
              (if (zero? n)
                0
                (pow 2 (dec n))))}))

(defn read-cards [f]
  (map parse-card (str/split-lines (slurp (io/resource f)))))

(reduce + (map :score (read-cards "d4_small.txt")))
(reduce + (map :score (read-cards "d4.txt")))

