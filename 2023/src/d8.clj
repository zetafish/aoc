(ns d8
  "Haunted Wasteland"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-code [s]
  (map {\L :left \R :right} s))

(defn parse-node [s]
  (let [[_ a b c] (re-matches #"(\w+) = \((\w+), (\w+)\)" s)]
    [a {:left b :right c}]))

(defn read-network [f]
  (let [coll (str/split-lines (slurp (io/resource f)))]
    {:code (parse-code (first coll))
     :nodes (into {} (map parse-node (drop 2 coll)))}))

(defn step [state dir]
  (assoc state :loc (get-in (:nodes state) [(:loc state) dir])))

(defn count-the-steps [network from to]
  (->> (reductions step
                   (assoc network :loc from)
                   (cycle (:code network)))
       (take-while #(not= to (:loc %)))
       count))

(count-the-steps (read-network "d8.txt") "AAA" "ZZZ")
