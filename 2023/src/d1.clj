(ns d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-number [s]
  (->> (filter (set "123456789") s)
       ((juxt first last))
       (str/join)
       (Integer/parseInt)))

(def data (str/split-lines (slurp (io/resource "d1.txt"))))

(def re-digit #("(zero|one|two|three|four|five|six|seven|eight|nine)"))

(defn parse-number-re [s]
  (let [digit "one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9"
        number (merge {"one" 1
                       "two" 2
                       "three" 3
                       "four" 4
                       "five" 5
                       "six" 6
                       "seven" 7
                       "eight" 8
                       "nine" 9}
                      (zipmap (map str "123456789") [1 2 3 4 5 6 7 8 9]))
        re (re-pattern (format ".*?(%s).*(%s).*" digit digit))
        m (re-matches re s)]
    (reduce (fn [a d] (+ (* 10 a) d)) 0
            (map number (drop 1 m)))))

(println (reduce + (map parse-number data)))

(println (reduce + (map parse-number-re data)))

(reduce + (map parse-number-re ["two1nine"
                                "eightwothree"
                                "abcone2threexyz"
                                "xtwone3four"
                                "4nineeightseven2"
                                "zoneight234"
                                "7pqrstsixteen"]))

